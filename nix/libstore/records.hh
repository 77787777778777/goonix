#pragma once

#include "types.hh"
#include "sqlite.hh"
#include <map>
#include <vector>

namespace nix {

struct BuildRecord {
    string resolvedHash;
    Path resolvedDerivation;
    string output;
    Path path;
    string origin;
    bool relocationChecked = false;
    string signature;
};

/* Domain-separated, length-prefixed bytes of every signed record field. */
string canonicalBuildRecordPayload(const BuildRecord & record);

typedef std::map<std::pair<Path, string>, Path> OutputChoices;

/* Permanent facts and local choices.  Deliberately independent of store
   rows: neither is lost when a .drv or an output is collected. */
class BuildRecords {
    sqlite3 * db;
public:
    BuildRecords(sqlite3 * db);
    void initialise();
    void add(const BuildRecord & record);
    std::vector<BuildRecord> query(const string & resolvedHash,
        const string & output = "");
    void choose(const Path & unresolved, const string & output,
        const Path & realised);
    Path choice(const Path & unresolved, const string & output);
    OutputChoices choices(const Path & unresolved);
};

}
