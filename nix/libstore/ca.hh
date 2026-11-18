// goonix: content-addressing tokens, tree hash, resolution and finalisation.
#pragma once

#include "types.hh"
#include "hash.hh"
#include "store-api.hh"
#include "derivations.hh"

namespace nix {

struct CAToken {
    string hash;
    string name;
    Path path;
    bool placeholder;
};

string selfToken(const string & output);
Path tokenPath(const string & output, const string & name);
Path makePlaceholder(const string & unresolvedBytes, const string & output,
    const string & name);
bool isReservedStorePath(const Path & path);
bool isPlaceholderPath(const Path & path);
bool isTokenStorePath(const Path & path);
void assertNotReserved(const Path & path);
string validateTokenPath(const string & path);

enum class Addressing { input, content, resolved };
Addressing addressingOf(const Derivation & drv);
bool goonixAddressed(const Derivation & drv);
bool resolvedDerivation(const Derivation & drv);

/* CA-4 mixed-policy safety (plan 3.1): an input-addressed derivation may
   have only input-addressed and fixed-output inputs.  As soon as one input
   is content- or resolved-addressed, the dependent must be too.  Throws if
   an input-addressed derivation directly references a CA derivation. */
void checkInputAddressedPolicy(class LocalStore & store, const Derivation & drv);

/* Validate the shape of a resolved derivation before it is registered:
   marked resolved, no floating inputs, outputs are self tokens (content)
   or strict final paths (resolved/fixed), environment agrees. */
void checkResolvedDerivation(const Derivation & drv);

/* Resolve every original derivation/output through daemon-owned choices.
   The resulting derivation contains only realised paths and self tokens. */
Derivation resolveDerivation(class LocalStore & store,
    const Path & unresolvedPath, const Derivation & unresolved);
Path writeResolvedDerivation(class LocalStore & store,
    const Path & originalPath, const Derivation & resolved);

Hash treeHash(const Path & path);
Path contentAddressedPath(const string & output, const string & name,
    const Hash & tree, const PathSet & references);
Path resolvedAddressedPath(const string & output, const string & name,
    const string & resolvedBytes);

/* Canonicalise and rewrite an entire filesystem tree, then prove the inverse
   mapping reproduces the original tree hash.  Returns the final path. */
Path finaliseContentOutput(class LocalStore & store, const Path & tokenPath,
    const string & output, const string & name, const PathSet & allPaths,
    PathSet & references, Hash & narHash, uint64_t & narSize);

}
