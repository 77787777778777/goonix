#include "records.hh"
#include "serialise.hh"
#include "util.hh"
#include <sqlite3.h>
#include <format>

namespace nix {

static void execute(sqlite3 * db, const string & sql)
{
    if (sqlite3_exec(db, sql.c_str(), 0, 0, 0) != SQLITE_OK)
        throwSQLiteError(db, "updating goonix build records");
}

BuildRecords::BuildRecords(sqlite3 * db) : db(db) { }

void BuildRecords::initialise()
{
    execute(db, R"sql(
create table if not exists GoonixBuildRecords (
    id integer primary key autoincrement not null,
    resolvedHash text not null,
    resolvedDerivation text not null,
    output text not null,
    path text not null,
    origin text not null,
    relocationChecked integer not null,
    signature text not null,
    unique(resolvedHash, output, path, origin, signature));
create index if not exists GoonixRecordLookup
    on GoonixBuildRecords(resolvedHash, output);
create table if not exists GoonixChoices (
    derivation text not null,
    output text not null,
    path text not null,
    primary key(derivation, output));
)sql");
}

string canonicalBuildRecordPayload(const BuildRecord & record)
{
    StringSink payload;
    const string domain("goonix-record-v1\0", 17);
    payload((const unsigned char *) domain.data(), domain.size());
    writeLongLong(1, payload);
    for (auto & field : {record.resolvedHash, record.resolvedDerivation,
             record.output, record.path, record.origin}) {
        writeLongLong(field.size(), payload);
        payload((const unsigned char *) field.data(), field.size());
    }
    writeLongLong(record.relocationChecked ? 1 : 0, payload);
    return std::move(payload.s);
}

void BuildRecords::add(const BuildRecord & record)
{
    if (record.resolvedHash.size() != 64 || record.output.empty() ||
        record.origin.empty() || record.signature.empty())
        throw Error("invalid or unsigned goonix build record");
    SQLiteStmt stmt;
    stmt.create(db, "insert or ignore into GoonixBuildRecords "
        "(resolvedHash, resolvedDerivation, output, path, origin, "
        "relocationChecked, signature) values (?, ?, ?, ?, ?, ?, ?);");
    stmt.use()(record.resolvedHash)(record.resolvedDerivation)
        (record.output)(record.path)(record.origin)
        (record.relocationChecked ? 1 : 0)(record.signature).exec();
}

std::vector<BuildRecord> BuildRecords::query(const string & resolvedHash,
    const string & output)
{
    SQLiteStmt stmt;
    stmt.create(db, "select resolvedHash, resolvedDerivation, output, path, "
        "origin, relocationChecked, signature from GoonixBuildRecords "
        "where resolvedHash = ? and (? = '' or output = ?) order by id;");
    auto use = stmt.use();
    use(resolvedHash)(output)(output);
    std::vector<BuildRecord> result;
    while (use.next())
        result.push_back({use.getStr(0), use.getStr(1), use.getStr(2),
            use.getStr(3), use.getStr(4), use.getInt(5) != 0, use.getStr(6)});
    return result;
}

void BuildRecords::choose(const Path & unresolved, const string & output,
    const Path & realised)
{
    SQLiteStmt stmt;
    stmt.create(db, "insert or ignore into GoonixChoices "
        "(derivation, output, path) values (?, ?, ?);");
    stmt.use()(unresolved)(output)(realised).exec();
}

Path BuildRecords::choice(const Path & unresolved, const string & output)
{
    SQLiteStmt stmt;
    stmt.create(db, "select path from GoonixChoices where derivation = ? and output = ?;");
    auto use = stmt.use();
    use(unresolved)(output);
    return use.next() ? use.getStr(0) : "";
}

OutputChoices BuildRecords::choices(const Path & unresolved)
{
    SQLiteStmt stmt;
    stmt.create(db, "select output, path from GoonixChoices where derivation = ?;");
    auto use = stmt.use();
    use(unresolved);
    OutputChoices result;
    while (use.next()) result[{unresolved, use.getStr(0)}] = use.getStr(1);
    return result;
}

}
