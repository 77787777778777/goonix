#include "ca.hh"
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

int main()
{
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
