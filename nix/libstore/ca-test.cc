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
    Path final = finaliseContentOutput(store, token, "out", "existing-output",
        PathSet{token}, references, narHash, narSize);
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
        PathSet{againToken}, references, narHash, narSize);
    check(again == final, "existing result is reused at the same address");
    check(readFile(final + "/reference") == final,
        "existing self-reference remains in final form");
    check(readFile(witness + "/linked") == final,
        "hard-linked content is not rewritten");
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

int main(int argc, char ** argv)
{
    if (argc == 2 && string(argv[1]) == "--placeholder-vector") {
        checkPlaceholderVector();
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
