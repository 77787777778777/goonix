Format vectors
==============

Generate vectors from a populated store without starting a daemon or
writing to that store:

  nix develop . -c sh daemon/test/vectors/generate.sh STORE OUTPUT GHC

OUTPUT must not exist.  It receives a copy of every `.drv`, streaming C++
SHA-256 hashes and lengths for every non-derivation store item, and a
bounded sample of complete C++-generated NARs.  The sample is capped at
268435456 bytes; its candidates cover regular, executable, symlink,
directory, empty-file, empty-directory and large-file nodes.  A manifest
and SHA256 checksum manifest are included.  Daemon lock and chroot state
are excluded; they are not store items.  STORE is read-only input and
must not be collected or modified.

The NAR dumper is linked from the baseline's own `archive.cc`,
`serialise.cc`, `util.cc` and `affinity.cc`.  The vectors are evidence for
HS-1's parser, printer and hash comparisons, not a substitute for those
checks.  Raw vectors and generated binaries stay outside the commit; the
selected inventory transcript is retained under `../transcripts`.

The manifest records the input store and the number and byte sizes of the
vectors.  A failed archive is fatal: no incomplete run is reported as a
complete whole-store input set.
