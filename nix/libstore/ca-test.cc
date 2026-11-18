// goonix: test vectors for tokens, placeholders and reserved paths.
#include "ca.hh"
#include "local-store.hh"
#include "globals.hh"
#include <cstdlib>
#include <iostream>
#include <format>

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

int main(int argc, char ** argv)
{
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
