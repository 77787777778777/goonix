// goonix: black-box tests for the out-of-process CA scan helper.
#include "util.hh"
#include <cstdlib>
#include <iostream>
#include <format>
#include <sys/wait.h>
#include <unistd.h>
using namespace nix;

static int run(const Path & helper, const Path & tree, const string & token)
{
    pid_t child = fork();
    if (child < 0) throw SysError("forking helper test");
    if (child == 0) {
        execl(helper.c_str(), helper.c_str(), tree.c_str(), token.c_str(), (char *) nullptr);
        _exit(127);
    }
    int status;
    if (waitpid(child, &status, 0) < 0) throw SysError("waiting for helper test");
    return status;
}

int main(int argc, char ** argv)
{
    if (argc != 2) {
        std::cerr << "usage: ca-scan-test HELPER\n";
        return 2;
    }
    Path helper = argv[1];
    string token = "ttteoeueuuottuuotutetetooueeoutu";
    unsigned failures = 0;
    for (auto format : {"gz", "xz", "bz2", "zip"}) {
        Path file = string("tests/fixtures/ca-self-token.") + format;
        int status = run(helper, file, token);
        if (!WIFEXITED(status) || WEXITSTATUS(status) != 1) {
            std::cerr << "FAIL " << format << " found status " << status << '\n';
            ++failures;
        } else std::cout << "PASS " << format << " helper found token\n";
        status = run(helper, file, "oooooooooooooooooooooooooooooooo");
        if (!WIFEXITED(status) || WEXITSTATUS(status) != 0) {
            std::cerr << "FAIL " << format << " bare non-token reserved bytes are clean " << status << '\n';
            ++failures;
        } else std::cout << "PASS " << format << " bare non-token reserved bytes are clean\n";
    }
    int status = run(helper, "tests/fixtures/ca-placeholder-boundary.gz", token);
    if (!WIFEXITED(status) || WEXITSTATUS(status) != 1) {
        std::cerr << "FAIL decoded placeholder boundary " << status << '\n';
        ++failures;
    } else std::cout << "PASS decoded placeholder boundary is found\n";
    status = run(helper, "/does/not/exist", token);
    if (!WIFEXITED(status) || WEXITSTATUS(status) != 2) {
        std::cerr << "FAIL helper error status " << status << '\n';
        ++failures;
    } else std::cout << "PASS helper reports errors distinctly\n";
    return failures ? EXIT_FAILURE : EXIT_SUCCESS;
}
