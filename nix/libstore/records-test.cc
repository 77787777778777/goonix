#include "records.hh"
#include "local-store.hh"
#include "globals.hh"
#include "util.hh"
#include <cstdlib>
#include <functional>
#include <iostream>
#include <format>
#include <sys/wait.h>

using namespace nix;

static void configureTestStore(const Path & root, const Path & guix)
{
    settings.guixProgram = guix;
    settings.nixStateDir = root;
    settings.nixStore = root + "/store";
    settings.nixDBPath = root + "/db";
    settings.nixConfDir = root + "/etc";
    settings.nixLogDir = root + "/log";
    settings.nixDaemonSocketFile = root + "/socket";
    settings.buildUsersGroup = "";
}

static int checkUnauthorizedImport(int argc, char ** argv)
{
    configureTestStore(argv[1], argv[2]);
    createDirs(settings.nixConfDir);
    chmod(settings.nixConfDir.c_str(), 0700);
    const string originalEtc = getEnv("GUIX_CONFIGURATION_DIRECTORY",
        settings.nixConfDir);
    writeFile(settings.nixConfDir + "/signing-key.sec",
        readFile(originalEtc + "/signing-key.sec"));
    writeFile(settings.nixConfDir + "/signing-key.pub",
        readFile(originalEtc + "/signing-key.pub"));
    writeFile(settings.nixConfDir + "/acl", "(acl)");
    chmod((settings.nixConfDir + "/signing-key.sec").c_str(), 0400);
    LocalStore store(false);
    try {
        store.recordGoonixBuild(argv[4], argv[5], argv[6], argv[7],
            "unauthorized-import", false, getEnv("GOONIX_TEST_SIGNATURE"));
    } catch (const AuthenticationError & e) {
        if (string(e.what()).find("unauthorized public key") != string::npos &&
            store.queryGoonixBuildRecords(argv[7], argv[5]).empty()) {
            std::cout << "goonix unauthorized record import: PASS\n";
            store.chooseGoonixOutput(
                "/gnu/store/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee-a.drv",
                "out", argv[4]);
            if (store.chosenGoonixOutput(
                    "/gnu/store/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee-a.drv",
                    "out") != argv[4] ||
                !store.chosenGoonixOutput(
                    "/gnu/store/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee-b.drv",
                    "out").empty()) {
                std::cerr << "goonix choice persistence: FAIL\n";
                return 1;
            }
            std::cout << "goonix record and choice persistence: PASS\n";
            return 0;
        }
        throw;
    }
    std::cerr << "goonix unauthorized record import: FAIL (accepted)\n";
    return 1;
}

int main(int argc, char ** argv)
{
    if (argc == 8 && string(argv[3]) == "--check-unauthorized-import")
        return checkUnauthorizedImport(argc, argv);
    if (argc != 3) {
        std::cerr << "usage: records-test DATABASE-DIRECTORY GUIX\n";
        return 2;
    }
    settings.guixProgram = argv[2];
    settings.nixStateDir = argv[1];
    settings.nixStore = settings.nixStateDir + "/store";
    settings.nixDBPath = settings.nixStateDir + "/db";
    settings.nixConfDir = settings.nixStateDir + "/etc";
    settings.nixLogDir = settings.nixStateDir + "/log";
    settings.nixDaemonSocketFile = settings.nixStateDir + "/socket";
    settings.buildUsersGroup = "";
    const string fixtureEtc = getEnv("GUIX_CONFIGURATION_DIRECTORY",
        settings.nixConfDir);
    createDirs(settings.nixConfDir);
    chmod(settings.nixConfDir.c_str(), 0700);
    writeFile(settings.nixConfDir + "/signing-key.sec",
        readFile(fixtureEtc + "/signing-key.sec"));
    writeFile(settings.nixConfDir + "/signing-key.pub",
        readFile(fixtureEtc + "/signing-key.pub"));
    chmod((settings.nixConfDir + "/signing-key.sec").c_str(), 0400);

    BuildRecord vector{"resolved", "derivation", "out", "path", "origin",
        true, "not-signed"};
    const string payload = canonicalBuildRecordPayload(vector);
    const string expectedPayload("goonix-record-v1\0"
        "\1\0\0\0\0\0\0\0"
        "\10\0\0\0\0\0\0\0" "resolved"
        "\12\0\0\0\0\0\0\0" "derivation"
        "\3\0\0\0\0\0\0\0" "out"
        "\4\0\0\0\0\0\0\0" "path"
        "\6\0\0\0\0\0\0\0" "origin"
        "\1\0\0\0\0\0\0\0", 104);
    if (payload != expectedPayload || printHash(hashString(htSHA256, payload)) !=
        "da6009a94a97ea3c50f7528283ec6cc5f0f8d35ff413e0f5b0b562fa561a5002") {
        std::cerr << "goonix canonical record payload vector: FAIL\n";
        return 1;
    }
    auto ambiguousA = vector;
    auto ambiguousB = vector;
    ambiguousA.output = "ou"; ambiguousA.path = "tpath";
    ambiguousB.output = "out"; ambiguousB.path = "path";
    if (canonicalBuildRecordPayload(ambiguousA) ==
        canonicalBuildRecordPayload(ambiguousB)) {
        std::cerr << "goonix canonical record field framing: FAIL\n";
        return 1;
    }
    std::cout << "goonix canonical record payload vectors: PASS\n";

    LocalStore store(false);
    store.initialiseGoonixRecords();

    const string text = "Derive([(\"out\",\"" + settings.nixStore +
        "/ttteoeueuuottuuotutetetooueeoutu-item\",\"\",\"\")],[],[]," +
        "\"x86_64-linux\",\"/bin/sh\",[],[(\"goonixAddressing\",\"content\"),(" +
        "\"goonixResolved\",\"1\")])";
    Path drv = store.addTextToStore("resolved.drv", text, PathSet());
    const string hash = printHash(hashString(htSHA256, text));
    store.recordBuild(drv, "out", drv, "local-test", false);
    auto records = store.queryGoonixBuildRecords(hash, "out");
    if (records.size() != 1 || records.at(0).path != drv ||
        records.at(0).signature.empty()) {
        std::cerr << "goonix signing helper with forced stderr: FAIL\n";
        return 1;
    }
    store.recordGoonixBuild(drv, "out", drv, hash, "local-test", false,
        records.at(0).signature);
    if (store.queryGoonixBuildRecords(hash, "out").size() != 1) {
        std::cerr << "goonix authorized record import: FAIL\n";
        return 1;
    }
    std::cout << "goonix signing helper with forced stderr: PASS\n";

    auto replayed = records.at(0);
    replayed.path = store.addTextToStore("replayed-output", "replayed", PathSet());
    try {
        store.recordGoonixBuild(replayed.resolvedDerivation, replayed.output,
            replayed.path, replayed.resolvedHash, replayed.origin,
            replayed.relocationChecked, replayed.signature);
        std::cerr << "goonix record signature path replay: FAIL (accepted)\n";
        return 1;
    } catch (const AuthenticationError &) {
        std::cout << "goonix record signature path replay: PASS\n";
    }

    std::vector<std::function<void(BuildRecord &)>> mutations = {
             [](BuildRecord & r) { r.output = "lib"; },
             [](BuildRecord & r) { r.origin = "replayed-origin"; },
             [](BuildRecord & r) { r.relocationChecked = true; },
             [](BuildRecord & r) { r.resolvedHash = std::string(64, '0'); }};
    for (auto & mutate : mutations) {
        auto tampered = records.at(0);
        mutate(tampered);
        try {
            store.recordGoonixBuild(tampered.resolvedDerivation, tampered.output,
                tampered.path, tampered.resolvedHash, tampered.origin,
                tampered.relocationChecked, tampered.signature);
            std::cerr << "goonix record signature field binding: FAIL\n";
            return 1;
        } catch (const AuthenticationError &) { }
    }
    std::cout << "goonix record signature field binding: PASS\n";

    auto wrongResolved = records.at(0);
    wrongResolved.resolvedDerivation = store.addTextToStore(
        "wrong-resolved.drv", "Derive([],[],[],\"x86_64-linux\",\"/bin/sh\",[],[])", PathSet());
    try {
        store.recordGoonixBuild(wrongResolved.resolvedDerivation,
            wrongResolved.output, wrongResolved.path, wrongResolved.resolvedHash,
            wrongResolved.origin, wrongResolved.relocationChecked,
            wrongResolved.signature);
        std::cerr << "goonix resolved content binding: FAIL\n";
        return 1;
    } catch (const AuthenticationError &) {
        std::cout << "goonix resolved content binding: PASS\n";
        std::cout.flush();
    }

    settings.nixStateDir += "/unauthorized";
    settings.nixStore = settings.nixStateDir + "/store";
    settings.nixDBPath = settings.nixStateDir + "/db";
    settings.nixConfDir = settings.nixStateDir + "/etc";
    settings.nixLogDir = settings.nixStateDir + "/log";
    settings.nixDaemonSocketFile = settings.nixStateDir + "/socket";
    /* The authenticator caches its ACL for its process lifetime.  The child
       prepares an empty ACL before opening its store and helper. */
    const string unauthorized = settings.nixStateDir;
    Strings args = {argv[0], unauthorized, argv[2],
        "--check-unauthorized-import", drv, "out", drv, hash};
    auto argvChild = stringsToCharPtrs(args);
    setenv("GOONIX_TEST_SIGNATURE", records.at(0).signature.c_str(), 1);
    execv(argv[0], argvChild.data());
    throw SysError("executing unauthorized-import test");
}
