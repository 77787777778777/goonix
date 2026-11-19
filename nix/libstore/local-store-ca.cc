// goonix: local-store record, choice and resolved-path implementation.
#include "local-store.hh"
#include "ca-auth.hh" // goonix: authentication remains in upstream's module
#include "ca.hh"
#include "derivations.hh"
#include "globals.hh"
#include "pathlocks.hh"
#include <format>

namespace nix {

Path LocalStore::chosenOutput(const Path & unresolved, const string & output)
{
    return goonixRecords->choice(unresolved, output);
}

OutputChoices LocalStore::choices(const Path & unresolved)
{
    return goonixRecords->choices(unresolved);
}

void LocalStore::chooseOutput(const Path & unresolved, const string & output,
    const Path & realised)
{
    goonixRecords->choose(unresolved, output, realised);
}

void LocalStore::recordBuild(const Path & resolvedPath,
    const string & output, const Path & path, const string & origin,
    bool relocationChecked)
{
    // goonix: a fact names a real, strict store path, never a token/placeholder.
    assertStorePathStrict(path);
    assertNotReserved(path);
    const string payload = unparseDerivation(readDerivation(resolvedPath));
    const string hash = printHash(hashString(htSHA256, payload));
    BuildRecord record{hash, resolvedPath, output, path, origin,
        relocationChecked, ""};
    record.signature = signBuildRecord(canonicalBuildRecordPayload(record));
    goonixRecords->add(record);
}

void LocalStore::recordGoonixBuild(const Path & resolvedPath,
    const string & output, const Path & path, const string & resolvedHash,
    const string & origin, bool relocationChecked, const string & signature)
{
    const string verifiedHash = verifyGoonixRecordSignature(*this, signature);
    BuildRecord record{resolvedHash, resolvedPath, output, path, origin,
        relocationChecked, signature};
    /* Authenticate first: an unsigned or unauthorized mapping is refused
       regardless of its other fields (plan 7, NixOS/nix#11393). */
    const string expected = printHash(hashString(htSHA256,
        canonicalBuildRecordPayload(record)));
    if (verifiedHash != expected)
        throw AuthenticationError(
            "goonix build record signature does not match its fields");
    // goonix: reject malformed or unresolved paths before trusting any field.
    if (!isStorePathStrict(path) || isReservedStorePath(path))
        throw AuthenticationError(
            "goonix build record does not name a valid realised store path");
    if (!isStorePathStrict(resolvedPath) || !isDerivation(resolvedPath))
        throw AuthenticationError(
            "goonix build record does not name a resolved derivation");
    const string actual = printHash(hashString(htSHA256,
        unparseDerivation(readDerivation(resolvedPath))));
    if (actual != resolvedHash)
        throw AuthenticationError(
            "goonix build record does not match resolved derivation content");
    goonixRecords->add(record);
}

std::vector<BuildRecord> LocalStore::queryGoonixBuildRecords(
    const string & hash, const string & output)
{
    return goonixRecords->query(hash, output);
}

std::pair<std::vector<BuildRecord>, Strings>
LocalStore::queryGoonixBuildGraph(const Path & unresolved)
{
    /* Resolve against choices already made and produce facts and their
       record-backed routing hints in one bottom-up walk. */
    std::vector<BuildRecord> result;
    Strings hints;
    PathSet visited;
    std::function<void(const Path &, const Derivation &)> visit =
        [&](const Path & originalPath, const Derivation & original) {
            if (!visited.insert(originalPath).second) return;
            for (auto & input : original.inputDrvs)
                visit(input.first, readDerivation(input.first));
            if (!goonixAddressed(original)) return;
            /* Names, not input availability, determine R.  A collected
               choice remains a valid name; no query may repair a choice. */
            auto nameOf = [&](const Path & path, const Derivation & drv,
                              const string & output) -> Path {
                auto found = drv.outputs.find(output);
                if (found == drv.outputs.end())
                    throw Error(std::format(
                        "derivation has no output `{0}'", output));
                if (!goonixAddressed(drv) || !found->second.hash.empty())
                    return found->second.path;
                return chosenOutput(path, output);
            };
            for (auto & input : original.inputDrvs) {
                Derivation dependency = readDerivation(input.first);
                for (auto & output : input.second)
                    if (nameOf(input.first, dependency, output).empty())
                        return;          // no choice: this member is unresolved
            }
            Derivation resolved = resolveDerivation(*this, originalPath,
                                                    original, nameOf);
            const string resolvedHash = printHash(hashString(htSHA256,
                unparseDerivation(resolved)));
            for (auto & output : original.outputs) {
                Path chosen = chosenOutput(originalPath, output.first);
                if (chosen.empty()) continue;
                auto records = queryGoonixBuildRecords(resolvedHash, output.first);
                result.insert(result.end(), records.begin(), records.end());
                if (std::any_of(records.begin(), records.end(),
                        [&](const BuildRecord & record) {
                            return record.output == output.first &&
                                record.path == chosen;
                        })) {
                    hints.push_back(originalPath);
                    hints.push_back(output.first);
                    hints.push_back(chosen);
                }
            }
        };
    visit(unresolved, readDerivation(unresolved));
    return {result, hints};
}

std::vector<BuildRecord> LocalStore::queryGoonixRecordsByPath(const Path & path)
{
    return goonixRecords->queryByPath(path);
}

std::vector<BuildRecord> LocalStore::queryGoonixRecordsByResolved(const Path & resolved)
{
    return goonixRecords->queryByResolvedDerivation(resolved);
}

std::vector<RecordConflict> LocalStore::queryGoonixConflicts(
    const string & hash, const string & output)
{
    return goonixRecords->conflicts(hash, output);
}

PathSet LocalStore::goonixDerivationsForPath(const Path & path)
{
    return goonixRecords->derivationsForPath(path);
}

PathSet LocalStore::goonixOriginalsForPath(const Path & path)
{
    return goonixRecords->originalsForPath(path);
}

PathSet LocalStore::goonixChosenPaths(const Path & unresolved)
{
    return goonixRecords->chosenPaths(unresolved);
}

/* Facts and choices are separate (plan 3.3).  A choice is set by the first
   successful build or substitution and changed only on purpose.  Here:
   - a valid existing choice is kept (default `keep-local' policy); a
     disagreeing record is still stored, which is the reproducibility check;
   - a collected (invalid) choice is repaired from any valid record;
   - `adopt' is the explicit `adopt-server' policy and replaces the choice.
   Returns the path this machine now uses, or "" if none is usable. */
Path LocalStore::selectGoonixOutput(const Path & unresolved,
    const string & output, const string & resolvedHash,
    const Path & preferred, bool adopt)
{
    Path current = chosenOutput(unresolved, output);
    bool currentValid = !current.empty() && isValidPath(current);

    /* Gather candidate facts for this resolved derivation/output. */
    auto records = goonixRecords->query(resolvedHash, output);

    if (adopt && !preferred.empty() && isValidPath(preferred)) {
        if (currentValid && current != preferred)
            printMsg(lvlInfo, std::format(
                "goonix: adopting `{0}' over local choice `{1}' for output `{2}' of `{3}' (disagreement recorded)",
                preferred, current, output, unresolved));
        goonixRecords->rechoose(unresolved, output, preferred);
        return preferred;
    }

    if (currentValid) {
        /* Keep the local choice.  A preferred result that differs is a
           disagreement; it is already stored as a fact by the caller's
           record import, so surface it but do not switch. */
        if (!preferred.empty() && preferred != current && isValidPath(preferred))
            printMsg(lvlInfo, std::format(
                "goonix: keeping local choice `{0}' for output `{1}' of `{2}'; server offered `{3}' (disagreement recorded)",
                current, output, unresolved, preferred));
        return current;
    }

    /* No usable choice: prefer the offered path, else the first valid fact. */
    if (!preferred.empty() && isValidPath(preferred)) {
        goonixRecords->rechoose(unresolved, output, preferred);
        return preferred;
    }
    for (auto & record : records)
        if (isValidPath(record.path)) {
            goonixRecords->rechoose(unresolved, output, record.path);
            return record.path;
        }
    /* Nothing valid: leave any stale choice in place but report none. */
    return "";
}

void LocalStore::chooseGoonixOutput(const Path & unresolved,
    const string & output, const Path & realised)
{
    chooseOutput(unresolved, output, realised);
}

Path LocalStore::chosenGoonixOutput(const Path & unresolved,
    const string & output)
{
    return chosenOutput(unresolved, output);
}

void LocalStore::initialiseGoonixRecords()
{
    goonixRecords = std::make_unique<BuildRecords>(db);
    goonixRecords->initialise();
}

Path LocalStore::addTextToStoreNoDerivationOutputs(const string & name,
    const string & contents, const PathSet & references)
{
    Path path = computeStorePathForText(name, contents, references);
    if (isValidPath(path)) return path;
    PathLocks lock{PathSet{path}};
    if (isValidPath(path)) return path;
    if (pathExists(path)) deletePath(path);
    writeFile(path, contents);
    canonicalisePathMetaData(path, -1);
    ValidPathInfo info;
    info.path = path;
    auto hash = hashPath(htSHA256, path);
    info.hash = hash.first;
    info.narSize = hash.second;
    info.references = references;
    registerValidPath(info);
    return path;
}

Path LocalStore::addResolvedDerivation(const string & name,
    const string & contents, const PathSet & references)
{
    auto parsed = parseDerivationText(contents);
    // goonix: a stored CA derivation is either unresolved (placeholder
    // outputs) or fully resolved; validate whichever shape it claims.
    if (resolvedDerivation(parsed))
        checkResolvedDerivation(parsed);
    else {
        for (auto & output : parsed.outputs)
            if (!isReservedStorePath(output.second.path) &&
                !isStorePathStrict(output.second.path))
                throw Error(std::format("invalid resolved output `{}'", output.second.path));
    }
    return addTextToStoreNoDerivationOutputs(name, contents, references);
}

Path LocalStore::realisedOutputPath(const Path & drvPath,
    const Derivation & drv, const string & output)
{
    auto found = drv.outputs.find(output);
    if (found == drv.outputs.end())
        throw Error(std::format("derivation has no output `{}'", output));
    if (found->second.hash != "" ||
        drv.env.find("goonixAddressing") == drv.env.end())
        return found->second.path;
    // goonix: the choice belongs to this original derivation, not another D.
    Path path = chosenOutput(drvPath, output);
    if (!path.empty() && isValidPath(path))
        return path;
    // goonix: a collected choice can be re-realised from a permanent fact.
    // Resolve against this machine's choices and consult the records for R.
    Derivation resolved = resolveDerivation(*this, drvPath, drv);
    const string hash = printHash(hashString(htSHA256,
        unparseDerivation(resolved)));
    Path selected = selectGoonixOutput(drvPath, output, hash, "", false);
    if (selected.empty())
        throw Error(std::format("derivation output `{}' of `{}' is not realised",
            output, drvPath));
    return selected;
}

}
