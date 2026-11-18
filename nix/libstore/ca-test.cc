// goonix: test vectors for tokens, placeholders and reserved paths.
#include "ca.hh"
#include "references.hh"
#include "local-store.hh"
#include "globals.hh"
#include <cstdlib>
#include <iostream>
#include <format>

#include <fcntl.h>
#include <sys/stat.h>

#include <sys/resource.h>
#include <poll.h>

using namespace nix;
namespace nix { void checkRewriteForTest(const Path & path, const string & from, const string & to); }

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
    createSymlink(baseNameOf(output), output);
    PathSet references;
    Hash narHash;
    uint64_t narSize = 0;
    chmod(settings.nixStore.c_str(), 0755);
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
    writeFile(directory + "/reference", directory);
    const timespec times[2] = {{0, UTIME_OMIT}, {1, 0}};
    if (utimensat(AT_FDCWD, (directory + "/self").c_str(), times, AT_SYMLINK_NOFOLLOW) ||
        utimensat(AT_FDCWD, (directory + "/reference").c_str(), times, 0) ||
        utimensat(AT_FDCWD, directory.c_str(), times, 0))
        throw SysError("canonicalising CA test timestamps");
    references.clear();
    final = finaliseContentOutput(store, directory, "out", "symlink-directory",
        PathSet{directory}, references, narHash, narSize);
    if (stat(final.c_str(), &st)) throw SysError("reading final directory mode");
    check((st.st_mode & 07777) == 0755, "nested symlink preserves parent mode");
    check(st.st_mtime == 1, "rewritten directory retains canonical timestamp");
    if (lstat((final + "/self").c_str(), &st)) throw SysError("reading rewritten symlink");
    check(st.st_mtime == 1, "rewritten symlink retains canonical timestamp");
    if (stat((final + "/reference").c_str(), &st)) throw SysError("reading rewritten file");
    check(st.st_mtime == 1, "rewritten file retains canonical timestamp");
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

static void checkExistingItemIsUntouched(const Path & root)
{
    configureIsolatedStore(root);
    LocalStore store(false);
    createDirs(settings.nixStore);
    Path token = tokenPath("out", "existing-output");
    createDirs(token);
    writeFile(token + "/reference", token);
    PathSet references;
    Hash narHash;
    uint64_t narSize = 0;
    bool created = false;
    Path final = finaliseContentOutput(store, token, "out", "existing-output",
        PathSet{token}, references, narHash, narSize, {}, "", &created);
    check(created, "a newly moved final tree is owned by its finaliser");
    ValidPathInfo valid;
    valid.path = final;
    valid.hash = narHash;
    valid.narSize = narSize;
    valid.references = references;
    store.registerValidPath(valid);
    Path witness = root + "/witness";
    createDirs(witness);
    writeFile(witness + "/reference", final);
    if (link((final + "/reference").c_str(), (witness + "/linked").c_str()))
        throw SysError("hard-linking early-cutoff witness");
    chmod(final.c_str(), 0555);
    Path againToken = tokenPath("out", "existing-output");
    createDirs(againToken);
    writeFile(againToken + "/reference", againToken);
    Path again = finaliseContentOutput(store, againToken, "out", "existing-output",
        PathSet{againToken}, references, narHash, narSize, {}, "", &created);
    check(!created, "an existing valid final tree is not caller-owned");
    check(again == final, "existing result is reused at the same address");
    check(readFile(final + "/reference") == final,
        "existing self-reference remains in final form");
    check(readFile(witness + "/linked") == final,
        "hard-linked content is not rewritten");

    /* Put a token-form tree at the valid final address.  The old inverse
       check hashes that tree on disk and accepts it; the fixed check must
       refuse it without touching the item or a hard link to its contents. */
    if (chmod((final + "/reference").c_str(), 0600) ||
        chmod((witness + "/linked").c_str(), 0600))
        throw SysError("preparing the same-address collision fixture");
    writeFile(final + "/reference", againToken);
    check(readFile(witness + "/linked") == againToken,
        "collision fixture changed the linked existing item");
    bool collision = throws([&] {
        PathSet ignoredReferences;
        Hash ignoredNAR;
        uint64_t ignoredSize = 0;
        finaliseContentOutput(store, againToken, "out", "existing-output",
            PathSet{againToken}, ignoredReferences, ignoredNAR, ignoredSize);
    });
    check(collision, "corrupt tree at a claimed content address is refused");
    check(readFile(witness + "/linked") == againToken,
        "failed early-cutoff comparison leaves the linked item untouched");
    check(readFile(final + "/reference") == againToken,
        "failed early-cutoff comparison leaves the existing item untouched");
}

static void checkInvalidLeftoverIsReplaced(const Path & root)
{
    configureIsolatedStore(root);
    LocalStore store(false);
    createDirs(settings.nixStore);
    Path token = tokenPath("out", "leftover-output");
    createDirs(token);
    writeFile(token + "/reference", token);
    PathSet references;
    Hash narHash;
    uint64_t narSize = 0;
    Path final = finaliseContentOutput(store, token, "out", "leftover-output",
        PathSet{token}, references, narHash, narSize);
    /* This simulates a daemon death after the rename but before registration.
       The final tree has the right content but is not valid. */
    check(!store.isValidPath(final), "leftover fixture is not registered");
    createDirs(token);
    writeFile(token + "/reference", token);
    bool created = false;
    Path recovered = finaliseContentOutput(store, token, "out", "leftover-output",
        PathSet{token}, references, narHash, narSize, {}, "", &created);
    check(recovered == final, "leftover is recovered at its content address");
    check(created, "recovered leftover belongs to this finalisation");
    check(readFile(final + "/reference") == final,
        "recovered leftover contains the final self reference");
}

static void checkSiblingOutputReferences(const Path & root)
{
    configureIsolatedStore(root);
    LocalStore store(false);
    createDirs(settings.nixStore);
    Path libToken = tokenPath("lib", "multi-lib");
    createDirs(libToken);
    PathSet references;
    Hash narHash;
    uint64_t narSize = 0;
    Path libFinal = finaliseContentOutput(store, libToken, "lib", "multi-lib",
        PathSet{libToken}, references, narHash, narSize);
    Path outToken = tokenPath("out", "multi-out");
    createDirs(outToken);
    writeFile(outToken + "/uses-lib", libToken);
    references.clear();
    Path outFinal = finaliseContentOutput(store, outToken, "out", "multi-out",
        PathSet{outToken, libToken, libFinal}, references, narHash, narSize,
        std::map<Path, Path>{{libToken, libFinal}});
    check(readFile(outFinal + "/uses-lib") == libFinal,
        "sibling token is replaced by its final output path");
    check(references.count(libFinal) == 1,
        "sibling output is registered as a reference");
}

static void checkFinalRootBeforeExistence(const Path & root)
{
    configureIsolatedStore(root);
    createDirs(settings.nixStore);
    Path token = tokenPath("out", "root-before-existence");
    createDirs(token);
    writeFile(token + "/reference", token);
    HashResult ignored;
    PathSet initialReferences = scanForReferences(token, PathSet{token}, ignored);
    initialReferences.erase(token);
    Path final = contentAddressedPath("out", "root-before-existence",
        treeHash(token), initialReferences);
    Pipe ready, proceed;
    ready.create();
    proceed.create();
    pid_t pid = fork();
    if (pid == -1) throw SysError("forking CA rooting test");
    if (pid == 0) {
        try {
            ready.readSide.close();
            proceed.writeSide.close();
            /* The store and its roots file belong to this child, not to the
               parent acting as the collector. This avoids inherited POSIX
               lock ownership. */
            LocalStore store(false);
            store.addTempRoot(token);
            Path roots = settings.nixStateDir + "/temproots/" +
                std::to_string(getpid());
            writeFull(ready.writeSide, roots + "\n");
            char byte;
            if (read(proceed.readSide, &byte, 1) != 1)
                throw Error("rooting test did not receive proceed signal");
            PathSet references;
            Hash narHash;
            uint64_t narSize = 0;
            Path result = finaliseContentOutput(store, token, "out",
                "root-before-existence", PathSet{token}, references,
                narHash, narSize);
            check(result == final, "rooting fixture selected expected final path");
            check(references.count(final) == 1,
                "rooting fixture includes its final self reference");
            _exit(failures == 0 ? EXIT_SUCCESS : EXIT_FAILURE);
        } catch (const std::exception & e) {
            try { std::cerr << "rooting child: " << e.what() << '\n'; } catch (...) { }
            _exit(EXIT_FAILURE);
        }
    }
    Pid child = pid;
    ready.writeSide.close();
    proceed.readSide.close();
    Path roots = readLine(ready.readSide);
    check(!roots.empty(), "rooting child created its temporary roots file");
    AutoCloseFD lock = openLockFile(roots, false);
    if (!lockFile(lock, ltRead, true)) throw Error("locking collector roots");
    writeFull(proceed.writeSide, "go\n");
    pollfd completion = {ready.readSide.borrow(), POLLIN | POLLHUP, 0};
    int readyResult = poll(&completion, 1, 1000);
    string contents = readFile(roots, true);
    bool rooted = false;
    size_t at = 0;
    while (at < contents.size()) {
        size_t end = contents.find('\0', at);
        if (end == string::npos) break;
        if (contents.substr(at, end - at) == final) rooted = true;
        at = end + 1;
    }
    check(readyResult == 0 && pathExists(token),
        "finalisation waits for the temporary-roots read lock");
    /* A collector holds this lock while taking its snapshot. addTempRoot
       excludes it before the final path can be checked, moved or reused. */
    check(!rooted,
        "final path cannot enter the store while the collector snapshot is blocked");
    lockFile(lock, ltNone, true);
    check(child.wait(true) == 0, "rooting child completes after collector lock releases");
    contents = readFile(roots, true);
    check(contents.find(final + '\0') != string::npos,
        "final path is rooted before the finaliser can complete");
    check(readFile(final + "/reference") == final,
        "rooted final tree is intact after the lock handoff");
}

static void checkPlaceholderVector()
{
    Derivation drv;
    drv.platform = "x86_64-linux";
    drv.builder = "/bin/sh";
    drv.env["goonixAddressing"] = "content";
    drv.outputs["lib"] = DerivationOutput("", "", "");
    drv.outputs["out"] = DerivationOutput("", "", "");
    drv.env["lib"] = "";
    drv.env["out"] = "";
    settings.nixStore = "/gnu/store";
    const string bytes = unparseDerivation(drv);
    Path actual = makePlaceholder(bytes, "lib", "ca-multi-lib");
    const Path expected = "/gnu/store/eeoeooettuoueuuooetuooteouteeooe-ca-multi-lib";
    if (actual != expected) std::cerr << "actual placeholder=" << actual << "\n";
    check(actual == expected,
        "placeholder bytes match the Scheme serializer");
}

static void checkTreeVectors()
{
    Path root = "/tmp/goonix-ca-vectors";
    if (pathExists(root)) deletePath(root);
    createDirs(root);
    writeFile(root + "/empty", "");
    chmod((root + "/empty").c_str(), 0444);
    Hash empty = treeHash(root + "/empty");
    check(printHash(empty) ==
        "e9af47ca501ef1fa917417af3a6b6943e227a92536e97037c23c2c673b45ddf2",
        "empty file tree vector");
    createSymlink("target", root + "/link");
    check(printHash(treeHash(root + "/link")) ==
        "5ef2f8b90d0ca186d4b49cc6bbf69533d7bfe13bdc86bd4e7a2ef354ffb6812c",
        "symlink tree vector");
    createDirs(root + "/dir");
    writeFile(root + "/dir/empty", "");
    chmod((root + "/dir/empty").c_str(), 0444);
    check(printHash(treeHash(root + "/dir")) ==
        "4d906c085e9d72c6732872daeafd12323257f240329e76da0e9550d1ec24a2dd",
        "directory tree vector");
    Path actualContent = contentAddressedPath("out", "hello", empty, {});
    check(actualContent ==
        "/gnu/store/kq0f52qvhdwr2a7a5ycvrra8b124sdw4-hello",
        "content path vector");
}

static void checkStagedToken(const Path & root)
{
    configureIsolatedStore(root);
    LocalStore store(false);
    createDirs(settings.nixStore);
    Path token = tokenPath("out", "ca-relocation");
    Path staged = settings.nixStore + "/.goonix-tmp-test";
    createDirs(staged);
    writeFile(staged + "/reference", token);
    createSymlink(baseNameOf(token), staged + "/link");
    createDirs(staged + "/" + baseNameOf(token));
    writeFile(staged + "/executable", "#!/bin/sh\n");
    chmod((staged + "/executable").c_str(), 0555);
    PathSet references;
    Hash narHash;
    uint64_t narSize = 0;
    Path final = finaliseContentOutput(store, token, "out", "ca-relocation",
        PathSet{token}, references, narHash, narSize, {}, staged);
    check(readFile(final + "/reference") == final,
        "staged physical path retains the logical self token");
    check(readLink(final + "/link") == baseNameOf(final),
        "staged symlink is rewritten against the logical token");
}

static void checkSiblingOrder(const Path & root)
{
    configureIsolatedStore(root);
    LocalStore store(false);
    createDirs(settings.nixStore);
    Derivation drv;
    drv.outputs["lib"] = DerivationOutput(tokenPath("lib", "multi-lib"), "", "");
    drv.outputs["out"] = DerivationOutput(tokenPath("out", "multi-out"), "", "");
    Path out = drv.outputs["out"].path;
    createDirs(out);
    writeFile(out + "/uses-lib", drv.outputs["lib"].path);
    createDirs(drv.outputs["lib"].path);
    auto order = contentOutputOrder(drv, PathSet{out, drv.outputs["lib"].path});
    check(order.size() == 2 && order.front().first == "lib",
        "referenced sibling is finalised before its dependent");
}

static void checkOpaqueFragmentBoundary(const Path & root)
{
    configureIsolatedStore(root);
    Path token = tokenPath("out", "fragment");
    Path file = root + "/fragment";
    string hash = baseNameOf(token).substr(0, 32);
    size_t split = 128 * 1024 - 31;
    string contents(split, 'x');
    contents += hash.substr(0, 1);
    contents += hash.substr(1, 7);
    contents += "tail";
    writeFile(file, contents);
    check(throws([&] { checkRewriteForTest(file, hash, hash); }),
        "opaque fragment split across a read boundary is refused");
}

static void checkInverseFragmentBoundary(const Path & root)
{
    configureIsolatedStore(root);
    Path file = root + "/inverse-fragment";
    string hash = baseNameOf(tokenPath("out", "inverse")).substr(0, 32);
    string contents(128 * 1024 - 31, 'x');
    contents += hash.substr(0, 1);
    contents += hash.substr(1, 7);
    contents += "tail";
    writeFile(file, contents);
    check(throws([&] { checkRewriteForTest(file, hash, hash); }),
        "inverse fragment split across a read boundary is refused");
}

int main(int argc, char ** argv)
{
    if (argc == 3 && string(argv[1]) == "--inverse-fragment") {
        checkInverseFragmentBoundary(argv[2]);
        return failures == 0 ? EXIT_SUCCESS : EXIT_FAILURE;
    }
    if (argc == 3 && string(argv[1]) == "--fragment-boundary") {
        checkOpaqueFragmentBoundary(argv[2]);
        return failures == 0 ? EXIT_SUCCESS : EXIT_FAILURE;
    }
    if (argc == 3 && string(argv[1]) == "--sibling-order") {
        checkSiblingOrder(argv[2]);
        return failures == 0 ? EXIT_SUCCESS : EXIT_FAILURE;
    }
    if (argc == 3 && string(argv[1]) == "--staged") {
        checkStagedToken(argv[2]);
        return failures == 0 ? EXIT_SUCCESS : EXIT_FAILURE;
    }
    if (argc == 2 && string(argv[1]) == "--tree-vectors") {
        settings.nixStore = "/gnu/store";
        checkTreeVectors();
        return failures == 0 ? EXIT_SUCCESS : EXIT_FAILURE;
    }
    if (argc == 2 && string(argv[1]) == "--placeholder-vector") {
        checkPlaceholderVector();
        return failures == 0 ? EXIT_SUCCESS : EXIT_FAILURE;
    }
    if (argc == 3 && string(argv[1]) == "--leftover") {
        checkInvalidLeftoverIsReplaced(argv[2]);
        return failures == 0 ? EXIT_SUCCESS : EXIT_FAILURE;
    }
    if (argc == 3 && string(argv[1]) == "--final-root") {
        checkFinalRootBeforeExistence(argv[2]);
        return failures == 0 ? EXIT_SUCCESS : EXIT_FAILURE;
    }
    if (argc == 3 && string(argv[1]) == "--siblings") {
        checkSiblingOutputReferences(argv[2]);
        return failures == 0 ? EXIT_SUCCESS : EXIT_FAILURE;
    }
    if (argc == 3 && string(argv[1]) == "--existing-item") {
        checkExistingItemIsUntouched(argv[2]);
        return failures == 0 ? EXIT_SUCCESS : EXIT_FAILURE;
    }
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
    if (argc != 1) {
        /* A check that does not exist must not pass as the vectors below. */
        std::cerr << "ca-test: unknown check `" << argv[1] << "'\n";
        return EXIT_FAILURE;
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
