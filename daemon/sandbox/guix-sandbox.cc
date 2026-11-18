/* GNU Guix --- Functional package management for GNU
   Copyright (C) 2025 Caleb Ristvedt <reepca@russelstein.xyz>

   This file is part of GNU Guix.

   GNU Guix is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or (at
   your option) any later version.

   GNU Guix is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.  */

/* Initial standalone plan interpreter derived from nix/libutil/spawn.cc.
   See README.md for the intentionally narrow scope and source relation.  */

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#ifndef __linux__
#error "the initial user namespace helper requires Linux"
#endif

#include <algorithm>
#include <cerrno>
#include <climits>
#include <csignal>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <exception>
#include <fcntl.h>
#include <grp.h>
#include <sched.h>
#include <set>
#include <stdexcept>
#include <string>
#include <sys/prctl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <utility>
#include <vector>

namespace {

using Bytes = std::string;

constexpr size_t maxPlanSize = 1024 * 1024;
constexpr uint32_t noFd = UINT32_MAX;

constexpr uint32_t setIdsFlag = 1;
constexpr uint32_t setSidFlag = 2;
constexpr uint32_t closeFdsFlag = 4;
constexpr uint32_t newUserNsFlag = 8;
constexpr uint32_t inheritEnvFlag = 16;
constexpr uint32_t chdirFlag = 32;
constexpr uint32_t stdinFileFlag = 64;
constexpr uint32_t stdinFdFlag = 128;
constexpr uint32_t clearGroupsFlag = 256;
constexpr uint32_t noNewPrivsFlag = 512;
constexpr uint32_t setupSuccessFlag = 1024;
constexpr uint32_t redirectLogFlag = 2048;
constexpr uint32_t knownFlags = setIdsFlag | setSidFlag | closeFdsFlag |
    newUserNsFlag | inheritEnvFlag | chdirFlag | stdinFileFlag |
    stdinFdFlag | clearGroupsFlag | noNewPrivsFlag | setupSuccessFlag |
    redirectLogFlag;

class Error : public std::runtime_error {
public:
    explicit Error(const std::string & message) : std::runtime_error(message) { }
};

class SysError : public Error {
public:
    SysError(const std::string & message, int error)
        : Error(message + ": " + std::strerror(error)) { }
    explicit SysError(const std::string & message)
        : SysError(message, errno) { }
};

[[noreturn]] void sysError(const std::string & message)
{
    throw SysError(message);
}

struct Plan {
    uint32_t flags = 0;
    Bytes program;
    std::vector<Bytes> args;
    std::vector<std::pair<Bytes, Bytes>> env;
    Bytes cwd;
    Bytes stdinFile;
    uid_t uid = 0;
    gid_t gid = 0;
    int stdinFd = -1;
    int logFd = -1;
    std::vector<gid_t> supplementaryGroups;
    std::set<int> earlyCloseFds;
    uid_t uidMapInside = 0;
    uid_t uidMapOutside = 0;
    gid_t gidMapInside = 0;
    gid_t gidMapOutside = 0;
    size_t phase = 0;

    bool has(uint32_t flag) const { return (flags & flag) != 0; }
};

/* The plan is bounded before allocation and every string is checked for NUL,
   since the execution interface uses C strings but carries arbitrary other
   path and argument bytes. */
class PlanReader {
    const Bytes input;
    size_t offset = 0;

public:
    explicit PlanReader(Bytes bytes) : input(std::move(bytes)) { }

    uint32_t u32()
    {
        require(4);
        const auto * p = reinterpret_cast<const unsigned char *>(input.data() + offset);
        offset += 4;
        return uint32_t(p[0]) | (uint32_t(p[1]) << 8) |
            (uint32_t(p[2]) << 16) | (uint32_t(p[3]) << 24);
    }

    Bytes string()
    {
        uint32_t size = u32();
        require(size);
        Bytes result(input, offset, size);
        offset += size;
        if (result.find('\0') != Bytes::npos)
            throw Error("plan byte string contains NUL");
        return result;
    }

    uint32_t count(uint32_t value)
    {
        if (value > (input.size() - offset) / 4)
            throw Error("plan item count exceeds remaining input");
        return value;
    }

    int fd()
    {
        uint32_t value = u32();
        if (value == noFd) return -1;
        if (value > uint32_t(INT_MAX))
            throw Error("plan descriptor is out of range");
        return int(value);
    }

    void finish()
    {
        if (offset != input.size()) throw Error("trailing bytes in plan");
    }

private:
    void require(size_t size)
    {
        if (size > input.size() - offset) throw Error("truncated plan");
    }
};

void readFull(int fd, unsigned char * buffer, size_t count)
{
    while (count) {
        ssize_t result = read(fd, buffer, count);
        if (result == -1) {
            if (errno == EINTR) continue;
            sysError("reading from file descriptor");
        }
        if (result == 0) throw Error("unexpected end-of-file");
        count -= size_t(result);
        buffer += result;
    }
}

void writeFull(int fd, const unsigned char * buffer, size_t count)
{
    while (count) {
        ssize_t result = write(fd, buffer, count);
        if (result == -1) {
            if (errno == EINTR) continue;
            sysError("writing to file descriptor");
        }
        count -= size_t(result);
        buffer += result;
    }
}

void writeFull(int fd, const Bytes & value)
{
    writeFull(fd, reinterpret_cast<const unsigned char *>(value.data()), value.size());
}

void waitForMessage(int fd, const Bytes & message)
{
    Bytes actual(message.size(), '\0');
    readFull(fd, reinterpret_cast<unsigned char *>(actual.data()), actual.size());
    if (actual != message) throw Error("unexpected synchronization message");
}

class Fd {
    int value;
public:
    explicit Fd(int fd = -1) : value(fd) { }
    Fd(const Fd &) = delete;
    Fd & operator=(const Fd &) = delete;
    ~Fd() { if (value != -1) close(value); }
    int get() const { return value; }
    void reset(int fd = -1)
    {
        if (value != -1) close(value);
        value = fd;
    }
    int release()
    {
        int result = value;
        value = -1;
        return result;
    }
};

struct Pipe {
    Fd readSide;
    Fd writeSide;
};

Pipe makePipe()
{
    int fds[2];
    if (pipe2(fds, O_CLOEXEC) == -1) sysError("creating pipe");
    return Pipe { Fd(fds[0]), Fd(fds[1]) };
}

void writeFile(const Bytes & path, const Bytes & contents)
{
    Fd fd(open(path.c_str(), O_WRONLY | O_CLOEXEC));
    if (fd.get() == -1) sysError("opening " + path);
    writeFull(fd.get(), contents);
    if (close(fd.release()) == -1) sysError("closing " + path);
}

Bytes readPlan(const char * path)
{
    Fd fd(open(path, O_RDONLY | O_CLOEXEC | O_NOFOLLOW));
    if (fd.get() == -1) sysError(std::string("opening plan `") + path + "'");
    struct stat st;
    if (fstat(fd.get(), &st) == -1) sysError("examining plan");
    if (!S_ISREG(st.st_mode)) throw Error("plan is not a regular file");
    if (st.st_size < 0 || uint64_t(st.st_size) > maxPlanSize)
        throw Error("plan exceeds size limit");
    Bytes input(size_t(st.st_size), '\0');
    readFull(fd.get(), reinterpret_cast<unsigned char *>(input.data()), input.size());
    unsigned char extra;
    ssize_t result;
    do {
        result = read(fd.get(), &extra, 1);
    } while (result == -1 && errno == EINTR);
    if (result == -1) sysError("checking end of plan");
    if (result != 0) throw Error("plan grew while being read");
    return input;
}

/* Parsing starts after the fixed magic.  Keeping the magic out of PlanReader
   makes all subsequent fields use the same bounded u32/string primitives. */
Plan parseFields(const Bytes & input)
{
    static const Bytes magic("GSP0001\0", 8);
    if (input.size() < magic.size() || input.compare(0, magic.size(), magic) != 0)
        throw Error("invalid plan magic or version");
    PlanReader reader(input.substr(magic.size()));
    Plan plan;
    plan.flags = reader.u32();
    if ((plan.flags & ~knownFlags) != 0) throw Error("unknown plan flags");
    if (plan.has(stdinFileFlag) && plan.has(stdinFdFlag))
        throw Error("two stdin sources requested");

    plan.program = reader.string();
    if (plan.program.empty()) throw Error("empty program in plan");
    uint32_t argc = reader.count(reader.u32());
    for (uint32_t i = 0; i < argc; ++i) plan.args.push_back(reader.string());
    if (plan.args.empty()) throw Error("empty argv in plan");

    uint32_t envc = reader.count(reader.u32());
    std::set<Bytes> names;
    for (uint32_t i = 0; i < envc; ++i) {
        Bytes name = reader.string();
        Bytes value = reader.string();
        if (name.empty() || name.find('=') != Bytes::npos)
            throw Error("invalid environment variable name");
        if (!names.insert(name).second) throw Error("duplicate environment variable");
        plan.env.emplace_back(std::move(name), std::move(value));
    }

    plan.cwd = reader.string();
    plan.stdinFile = reader.string();
    plan.uid = uid_t(reader.u32());
    plan.gid = gid_t(reader.u32());
    plan.stdinFd = reader.fd();
    plan.logFd = reader.fd();
    if (plan.has(stdinFdFlag) && plan.stdinFd == -1)
        throw Error("fd-backed stdin has no descriptor");

    uint32_t groups = reader.count(reader.u32());
    for (uint32_t i = 0; i < groups; ++i)
        plan.supplementaryGroups.push_back(gid_t(reader.u32()));
    uint32_t earlyFds = reader.count(reader.u32());
    for (uint32_t i = 0; i < earlyFds; ++i) {
        int fd = reader.fd();
        if (fd < 0) throw Error("invalid early-close descriptor");
        if (fd <= STDERR_FILENO) throw Error("cannot early-close standard descriptor");
        plan.earlyCloseFds.insert(fd);
    }
    if ((plan.has(stdinFdFlag) && plan.earlyCloseFds.count(plan.stdinFd)) ||
        (plan.has(redirectLogFlag) && plan.earlyCloseFds.count(plan.logFd)))
        throw Error("redirected descriptor is also early-closed");
    if (plan.has(redirectLogFlag) && plan.logFd == -1)
        throw Error("log redirection has no descriptor");
    if (plan.has(stdinFdFlag) && plan.stdinFd <= STDERR_FILENO)
        throw Error("fd-backed stdin must use a nonstandard descriptor");
    if (plan.has(redirectLogFlag) && plan.logFd <= STDERR_FILENO)
        throw Error("log redirection must use a nonstandard descriptor");

    plan.uidMapInside = uid_t(reader.u32());
    plan.uidMapOutside = uid_t(reader.u32());
    plan.gidMapInside = gid_t(reader.u32());
    plan.gidMapOutside = gid_t(reader.u32());
    reader.finish();
    return plan;
}

Plan readAndParsePlan(const char * path)
{
    return parseFields(readPlan(path));
}

void reap(pid_t child)
{
    int status;
    pid_t result;
    do {
        result = waitpid(child, &status, 0);
    } while (result == -1 && errno == EINTR);
    if (result == -1) sysError("reaping user namespace map writer");
    if (!WIFEXITED(status) || WEXITSTATUS(status) != EXIT_SUCCESS)
        throw Error("user namespace map writer failed");
}

/* Retain spawn.cc's external map-writer handshake.  No daemon code runs
   after fork; the child only executes the C++ map-writing routine. */
void unshareAndInitUserns(Plan & plan)
{
    const pid_t target = getpid();
    const Bytes pid = std::to_string(target);
    Pipe toChild = makePipe();
    Pipe fromChild = makePipe();
    pid_t child = fork();
    if (child == -1) sysError("creating user namespace map writer");
    if (child == 0) {
        try {
            toChild.writeSide.reset();
            fromChild.readSide.reset();
            waitForMessage(toChild.readSide.get(), "ready\n");
            writeFile("/proc/" + pid + "/uid_map",
                std::to_string(plan.uidMapInside) + " " +
                std::to_string(plan.uidMapOutside) + " 1\n");
            writeFile("/proc/" + pid + "/setgroups", "deny\n");
            writeFile("/proc/" + pid + "/gid_map",
                std::to_string(plan.gidMapInside) + " " +
                std::to_string(plan.gidMapOutside) + " 1\n");
            writeFull(fromChild.writeSide.get(), "go\n");
        } catch (const std::exception & error) {
            try {
                writeFull(STDERR_FILENO,
                    Bytes("guix-sandbox: user namespace map writer: ") + error.what() + "\n");
            } catch (...) { }
            _exit(1);
        } catch (...) {
            _exit(1);
        }
        _exit(EXIT_SUCCESS);
    }

    toChild.readSide.reset();
    fromChild.writeSide.reset();
    try {
        if (unshare(CLONE_NEWUSER) == -1) sysError("unshare(CLONE_NEWUSER)");
        writeFull(toChild.writeSide.get(), "ready\n");
        waitForMessage(fromChild.readSide.get(), "go\n");
        toChild.writeSide.reset();
        fromChild.readSide.reset();
    } catch (...) {
        toChild.writeSide.reset();
        fromChild.readSide.reset();
        try { reap(child); } catch (...) { }
        throw;
    }
    reap(child);
}

using Action = void (*)(Plan &);
struct Phase { const char * label; Action action; };

void usernsSetIdsAction(Plan & plan)
{
    if (plan.has(newUserNsFlag)) {
        plan.uid = plan.uidMapInside;
        plan.gid = plan.gidMapInside;
    }
}

void setsidAction(Plan & plan)
{
    if (plan.has(setSidFlag) && setsid() == pid_t(-1))
        sysError("creating a new session");
}

void earlyIoSetupAction(Plan & plan)
{
    for (int fd : plan.earlyCloseFds)
        if (close(fd) == -1) sysError("closing fd");

    if (plan.has(redirectLogFlag)) {
        if (dup2(plan.logFd, STDOUT_FILENO) == -1)
            sysError("cannot dup2 log fd into stdout fd");
        if (dup2(plan.logFd, STDERR_FILENO) == -1)
            sysError("cannot dup2 log fd into stderr fd");
    }

    if (plan.has(stdinFdFlag)) {
        if (dup2(plan.stdinFd, STDIN_FILENO) == -1)
            sysError("cannot dup2 fd into stdin fd");
    } else if (plan.has(stdinFileFlag)) {
        Fd fd(open(plan.stdinFile.c_str(), O_RDWR | O_CLOEXEC));
        if (fd.get() == -1) sysError("cannot open stdin file");
        if (dup2(fd.get(), STDIN_FILENO) == -1)
            sysError("cannot dup2 fd into stdin fd");
    }
}

void chdirAction(Plan & plan)
{
    if (plan.has(chdirFlag) && chdir(plan.cwd.c_str()) == -1)
        sysError("changing working directory");
}

void closeMostFdsAction(Plan & plan)
{
    if (!plan.has(closeFdsFlag)) return;
    long maximum = sysconf(_SC_OPEN_MAX);
    if (maximum < 0) maximum = 65536;
    maximum = std::min(maximum, long(INT_MAX));
    for (int fd = STDERR_FILENO + 1; fd < maximum; ++fd)
        close(fd); /* Match closeMostFDs: absent descriptors are harmless. */
}

void setIdsAction(Plan & plan)
{
    if (plan.has(clearGroupsFlag) || plan.has(setIdsFlag)) {
        if (setgroups(plan.supplementaryGroups.size(),
                      plan.supplementaryGroups.data()) == -1)
            sysError("cannot set supplementary groups");
    }
    if (plan.has(setIdsFlag)) {
        if (setgid(plan.gid) == -1 || getgid() != plan.gid || getegid() != plan.gid)
            sysError("setgid failed");
        if (setuid(plan.uid) == -1 || getuid() != plan.uid || geteuid() != plan.uid)
            sysError("setuid failed");
    }
}

void setNoNewPrivsAction(Plan & plan)
{
    /* Upstream passes zero here; PR_SET_NO_NEW_PRIVS requires arg2 == 1. */
    if (plan.has(noNewPrivsFlag) && prctl(PR_SET_NO_NEW_PRIVS, 1, 0, 0, 0) == -1)
        sysError("setting PR_SET_NO_NEW_PRIVS");
}

void restoreSigpipeAction(Plan &)
{
    struct sigaction action { };
    action.sa_handler = SIG_DFL;
    sigemptyset(&action.sa_mask);
    if (sigaction(SIGPIPE, &action, nullptr) == -1) sysError("resetting SIGPIPE");
}

void setupSuccessAction(Plan & plan)
{
    if (plan.has(setupSuccessFlag)) writeFull(STDERR_FILENO, "\n");
}

std::vector<char *> stringPointers(std::vector<Bytes> & strings)
{
    std::vector<char *> result;
    result.reserve(strings.size() + 1);
    for (Bytes & value : strings) result.push_back(value.data());
    result.push_back(nullptr);
    return result;
}

void execAction(Plan & plan)
{
    std::vector<Bytes> envStrings;
    std::vector<char *> envPointers;
    char ** environment = nullptr;
    /* Reserve before taking pointers: a later vector reallocation would
       invalidate the C strings passed to execvpe. */
    envStrings.reserve(plan.env.size());
    envPointers.reserve(plan.env.size() + 1);
    if (plan.has(inheritEnvFlag)) {
        for (const auto & entry : plan.env)
            if (setenv(entry.first.c_str(), entry.second.c_str(), 1) == -1)
                sysError("setenv");
        environment = environ;
    } else {
        for (const auto & entry : plan.env) {
            envStrings.push_back(entry.first + "=" + entry.second);
            envPointers.push_back(envStrings.back().data());
        }
        envPointers.push_back(nullptr);
        environment = envPointers.data();
    }
    std::vector<char *> arguments = stringPointers(plan.args);
    execvpe(plan.program.c_str(), arguments.data(), environment);
    sysError("executing " + plan.program);
}

const std::vector<Phase> phases = {
    { "unshareAndInitUserns", [](Plan & p) { if (p.has(newUserNsFlag)) unshareAndInitUserns(p); } },
    { "usernsSetIDs", usernsSetIdsAction },
    { "setsid", setsidAction },
    { "earlyIOSetup", earlyIoSetupAction },
    { "chdir", chdirAction },
    { "closeMostFDs", closeMostFdsAction },
    { "setIDs", setIdsAction },
    { "setNoNewPrivs", setNoNewPrivsAction },
    { "restoreSIGPIPE", restoreSigpipeAction },
    { "setupSuccess", setupSuccessAction },
    { "exec", execAction },
};

[[noreturn]] void run(Plan plan)
{
    try {
        for (plan.phase = 0; plan.phase < phases.size(); ++plan.phase)
            phases[plan.phase].action(plan);
    } catch (const std::exception & error) {
        try {
            Bytes message = "guix-sandbox: child setup";
            if (plan.phase < phases.size())
                message += Bytes(": in phase ") + phases[plan.phase].label;
            message += ": ";
            message += error.what();
            message += "\n";
            writeFull(STDERR_FILENO, message);
        } catch (...) { }
        _exit(1);
    }
    _exit(1);
}

} // namespace

int main(int argc, char ** argv)
{
    if (argc != 2) {
        static const char usage[] = "usage: guix-sandbox PLAN\n";
        write(STDERR_FILENO, usage, sizeof(usage) - 1);
        return 1;
    }
    try {
        run(readAndParsePlan(argv[1]));
    } catch (const std::exception & error) {
        try {
            writeFull(STDERR_FILENO, Bytes("guix-sandbox: ") + error.what() + "\n");
        } catch (...) { }
        return 1;
    }
}
