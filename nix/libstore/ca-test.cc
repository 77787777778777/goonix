// goonix: test vectors for tokens, placeholders and reserved paths.
#include "ca.hh"
#include "local-store.hh"
#include "globals.hh"
#include <cstdlib>
#include <iostream>
#include <format>

#include <fcntl.h>
#include <sys/stat.h>

#include <sys/resource.h>

using namespace nix;

static unsigned failures = 0;

static void check(bool condition, const std::string & name)
{
    if (condition) {
        std::cout << "PASS " << name << '\n';
    } else {
        std::cerr << "FAIL " << name << '\n';
        ++failures;
    }
}

template<class F> static bool throws(F && f)
{
    try { f(); return false; }
    catch (const Error &) { return true; }
}

static void configureIsolatedStore(const Path & root)
{
    settings.nixStateDir = root + "/state";
    settings.nixStore = root + "/store";
    settings.nixDBPath = root + "/db";
    settings.nixConfDir = root + "/etc";
    settings.nixLogDir = root + "/log";
    settings.nixDaemonSocketFile = root + "/socket";
    settings.buildUsersGroup = "";
}

static void checkOutputPathValidation(const Path & root)
{
    configureIsolatedStore(root);
    LocalStore store(false);
    Hash hash = hashString(htSHA256, "attacker-controlled content");
    Derivation drv;
    drv.platform = "x86_64-linux";
    drv.builder = "/bin/sh";
    drv.env["goonixResolved"] = "1";
    drv.outputs["out"] = DerivationOutput(
        settings.nixStore + "/00000000000000000000000000000000-chosen",
        "r:sha256", printHash(hash));
    drv.env["out"] = drv.outputs["out"].path;
    bool accepted = true;
    try {
        store.addTextToStore("fixed.drv", unparseDerivation(drv), PathSet());
    } catch (const Error &) {
        accepted = false;
    }
    check(!accepted, "fixed output path is recomputed, not attacker chosen");

    drv.env.erase("goonixResolved");
    drv.outputs["out"] = DerivationOutput(
        settings.nixStore + "/00000000000000000000000000000001-chosen", "", "");
    drv.env["out"] = drv.outputs["out"].path;
    accepted = true;
    try { store.addTextToStore("unvalidated.drv", unparseDerivation(drv), PathSet()); }
    catch (const Error &) { accepted = false; }
    check(!accepted, "addressing marker cannot bypass output path validation");

    drv.env["goonixAddressing"] = "resolved";
    drv.env["goonixResolved"] = "1";
    drv.env["out"] = drv.outputs["out"].path;
    accepted = true;
    try { store.addTextToStore("resolved-chosen.drv", unparseDerivation(drv), PathSet()); }
    catch (const Error &) { accepted = false; }
    check(!accepted, "resolved-addressed path is recomputed from resolved bytes");

    drv.outputs["out"] = DerivationOutput(
        makePlaceholder(unparseDerivation(drv), "out", "placeholder"), "", "");
    drv.env["goonixResolved"] = "1";
    drv.env["out"] = drv.outputs["out"].path;
    accepted = true;
    try { store.addTextToStore("placeholder.drv", unparseDerivation(drv), PathSet()); }
    catch (const Error &) { accepted = false; }
    check(!accepted, "placeholder output path is bound to the unresolved derivation");
}

static void checkSymlinkModes(const Path & root)
{
    configureIsolatedStore(root);
    LocalStore store(false);
    createDirs(settings.nixStore);
    chmod(settings.nixStore.c_str(), 0755);
    Path output = tokenPath("out", "symlink-output");
    createSymlink(output, output);
    PathSet references;
    Hash narHash;
    uint64_t narSize = 0;
    Path final = finaliseContentOutput(store, output, "out", "symlink-output",
        PathSet{output}, references, narHash, narSize);
    struct stat st;
    if (stat(settings.nixStore.c_str(), &st))
        throw SysError("reading isolated store mode");
    check((st.st_mode & 07777) == 0755, "top-level symlink preserves store mode");
    Path directory = tokenPath("out", "symlink-directory");
    createDirs(directory);
    chmod(directory.c_str(), 0755);
    createSymlink(directory, directory + "/self");
    references.clear();
    final = finaliseContentOutput(store, directory, "out", "symlink-directory",
        PathSet{directory}, references, narHash, narSize);
    if (stat(final.c_str(), &st)) throw SysError("reading final directory mode");
    check((st.st_mode & 07777) == 0755, "nested symlink preserves parent mode");
}

static void checkStreamingFinalisation(const Path & root)
{
    configureIsolatedStore(root);
    LocalStore store(false);
    createDirs(settings.nixStore);
    Path output = tokenPath("out", "sparse-output");
    int fd = open(output.c_str(), O_WRONLY | O_CREAT | O_EXCL, 0600);
    if (fd < 0) throw SysError("creating sparse CA test output");
    AutoCloseFD close = fd;
    const uint64_t size = 128 * 1024 * 1024;
    if (ftruncate(fd, size)) throw SysError("sizing sparse CA test output");
    const string self = output;
    if (pwrite(fd, self.data(), self.size(), size / 2) != (ssize_t) self.size())
        throw SysError("writing sparse CA self-reference");
    close.close();
    struct rlimit limit;
    if (getrlimit(RLIMIT_AS, &limit)) throw SysError("reading address-space limit");
    if (limit.rlim_cur > 96 * 1024 * 1024) limit.rlim_cur = 96 * 1024 * 1024;
    if (setrlimit(RLIMIT_AS, &limit)) throw SysError("setting CA test memory limit");
    PathSet references;
    Hash narHash;
    uint64_t narSize = 0;
    Path final = finaliseContentOutput(store, output, "out", "sparse-output",
        PathSet{output}, references, narHash, narSize);
    check(pathExists(final), "sparse output finalises within the memory limit");
}

int main(int argc, char ** argv)
{
    if (argc == 3 && string(argv[1]) == "--streaming") {
        checkStreamingFinalisation(argv[2]);
        return failures == 0 ? EXIT_SUCCESS : EXIT_FAILURE;
    }
    if (argc == 3 && string(argv[1]) == "--symlink-modes") {
        checkSymlinkModes(argv[2]);
        return failures == 0 ? EXIT_SUCCESS : EXIT_FAILURE;
    }
    if (argc == 3 && string(argv[1]) == "--path-validation") {
        checkOutputPathValidation(argv[2]);
        return failures == 0 ? EXIT_SUCCESS : EXIT_FAILURE;
    }
    settings.nixStore = "/gnu/store";

    auto out = selfToken("out");
    check(out == "ttteoeueuuottuuotutetetooueeoutu", "self token vector");
    check(selfToken("lib") == "teuttuuueeuttoototeeeutooeeeuout", "lib token vector");
    check(tokenPath("out", "hello-1.0") == "/gnu/store/ttteoeueuuottuuotutetetooueeoutu-hello-1.0", "token path vector");
    check(out.size() == 32 && out[0] != 'e'
        && out.find_first_not_of("eout") == std::string::npos
        && out == selfToken("out"), "self token shape and determinism");
    check(out != selfToken("lib"), "self token depends on output");
    check(isTokenStorePath(tokenPath("out", "hello-1.0")),
        "token path has store shape");
    auto placeholder = makePlaceholder("drv bytes", "out", "hello-1.0");
    check(placeholder == "/gnu/store/eutuotetteeuuottttouuuteeutttuut-hello-1.0", "placeholder vector");
    check(isPlaceholderPath(placeholder), "placeholder has store shape");
    check(!isPlaceholderPath(tokenPath("out", "hello-1.0")),
        "self token is not placeholder");
    check(validateTokenPath(out + "-hello-1.0") == out + "-hello-1.0",
        "token basename validates");
    check(throws([] { validateTokenPath("eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee-hello"); }),
        "unknown placeholder fails closed");
    check(throws([] { validateTokenPath("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-hello"); }),
        "ordinary base32 is not a token");

    return failures == 0 ? EXIT_SUCCESS : EXIT_FAILURE;
}
