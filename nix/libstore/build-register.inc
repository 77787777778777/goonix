// goonix: build scheduler body moved out of the upstream file.
void DerivationGoal::registerOutputs()
{
    /* When using a build hook, the build hook can register the output
       as valid (by doing `nix-store --import').  If so we don't have
       to do anything here. */
    if (hook) {
        bool allValid = true;
        for (auto& i : drv.outputs)
            if (!worker.store.isValidPath(i.second.path)) allValid = false;
        if (allValid) return;
    }

    ValidPathInfos infos;

    /* Set of inodes seen during calls to canonicalisePathMetaData()
       for this build's outputs.  This needs to be shared between
       outputs to allow hard links between outputs. */
    InodesSeen inodesSeen;

    Path checkSuffix = "-check";

    // goonix: content finalisation is in build-ca.cc; ordinary outputs stay below.
    auto goonixResult = finaliseGoonixOutputs(worker.store, drv, originalDrv,
        drvPath, originalDrvPath, allPaths, missingPaths, useChroot, chrootRootDir,
        buildMode, buildUser.enabled() ? buildUser.getUID() : (uid_t) -1,
        buildUser.enabled() ? buildUser.getGID() : (gid_t) -1, inodesSeen,
        [&] { if (buildUser.enabled()) buildUser.kill(); });
    auto goonixFacts = goonixResult.facts;
    infos.splice(infos.end(), goonixResult.infos);
    auto discardGoonixOutputs = [&] { ::nix::discardGoonixOutputs(worker.store, goonixResult); };
    /* Check whether the output paths were created, and grep each
       output path to determine what other paths it references.  Also make all
       output paths read-only. */
    for (auto& i : drv.outputs) {
        Path path = i.second.path;
        if (missingPaths.find(path) == missingPaths.end() ||
            goonixResult.finalOutputs.count(path)) continue; // goonix
        Path actualPath = path;
        if (useChroot) {
            actualPath = chrootRootDir + path;
        } else {
            Path redirected = redirectedOutputs[path];
            if (buildMode == bmRepair
                && redirectedBadOutputs.find(path) != redirectedBadOutputs.end()
                && pathExists(redirected))
                replaceValidPath(path, redirected);
            if (buildMode == bmCheck && redirected != "")
                actualPath = redirected;
        }

        struct stat st;
        if (lstat(actualPath.c_str(), &st) == -1) {
            if (errno == ENOENT)
                throw BuildError(
                    std::format("builder for `{}' failed to produce output path `{}'",
                        drvPath, path));
            throw SysError(std::format("getting attributes of path `{}'", actualPath));
        }

#ifndef __CYGWIN__
        /* Check that the output is not group or world writable, as
           that means that someone else can have interfered with the
           build.  Also, the output should be owned by the build
           user. */
        if ((!S_ISLNK(st.st_mode) && (st.st_mode & (S_IWGRP | S_IWOTH))) ||
            (buildUser.enabled() && st.st_uid != buildUser.getUID()))
            throw BuildError(std::format("suspicious ownership or permission on `{}'; rejecting this build output", path));
#endif

        /* Apply hash rewriting if necessary. */
        bool rewritten = false;
        if (!rewritesFromTmp.empty()) {
            printMsg(lvlError, std::format("warning: rewriting hashes in `{}'; cross fingers", path));

            /* Canonicalise first.  This ensures that the path we're
               rewriting doesn't contain a hard link to /etc/shadow or
               something like that. */
            canonicalisePathMetaData(actualPath, buildUser.enabled() ? buildUser.getUID() : -1, inodesSeen);

            /* FIXME: this is in-memory. */
            StringSink sink;
            dumpPath(actualPath, sink);
            deletePath(actualPath);
            sink.s = rewriteHashes(sink.s, rewritesFromTmp);
            StringSource source(sink.s);
            restorePath(actualPath, source);

            rewritten = true;
        }

        startNest(nest, lvlTalkative,
            std::format("scanning for references inside `{}'", path));

        /* Check that fixed-output derivations produced the right
           outputs (i.e., the content hash should match the specified
           hash). */
        if (i.second.hash != "") {

            bool recursive; HashType ht; Hash h;
            i.second.parseHashInfo(recursive, ht, h);

            if (!recursive) {
                /* The output path should be a regular file without
                   execute permission. */
                if (!S_ISREG(st.st_mode) || (st.st_mode & S_IXUSR) != 0)
                    throw BuildError(
                        std::format("output path `{}' should be a non-executable regular file", path));
            }

            /* Check the hash. */
            Hash h2 = recursive ? hashPath(ht, actualPath).first : hashFile(ht, actualPath);
            if (h != h2) {
		if (settings.printBuildTrace)
		    printMsg(lvlError, std::format("@ hash-mismatch {} {} {} {}",
			    path, i.second.hashAlgo,
                printHash16or32(h), printHash16or32(h2)));
                throw BuildError(std::format("hash mismatch for store item '{}'", path));
	    }
        }

        /* Get rid of all weird permissions.  This also checks that
           all files are owned by the build user, if applicable. */
        canonicalisePathMetaData(actualPath,
            buildUser.enabled() && !rewritten ? buildUser.getUID() : -1, inodesSeen);

        if (useChroot) {
          if (pathExists(actualPath)) {
            /* Now that output paths have been canonicalized (in particular
               there are no setuid files left), move them outside of the
               chroot and to the store. */
            if (buildMode == bmRepair)
              replaceValidPath(path, actualPath);
            else
		if (buildMode != bmCheck) {
		    if (S_ISDIR(st.st_mode)) {
                        if (lstat(actualPath.c_str(), &st) == -1)
                            throw SysError(std::format("getting canonicalized permissions of directory `{}'", actualPath));
			/* Change mode on the directory to allow for
			   rename(2).  */
			if (chmod(actualPath.c_str(), st.st_mode | 0700) == -1)
                            throw SysError(std::format("making `{}' writable for move from chroot to store", actualPath));
                    }
		    if (rename(actualPath.c_str(), path.c_str()) == -1)
			throw SysError(std::format("moving build output `{}' from the chroot to the store", path));
		    if (S_ISDIR(st.st_mode) && chmod(path.c_str(), st.st_mode) == -1)
			throw SysError(std::format("restoring permissions on directory `{}'", actualPath));
		}
          }
          if (buildMode != bmCheck) actualPath = path;
        }

        /* For this output path, find the references to other paths
           contained in it.  Compute the SHA-256 NAR hash at the same
           time.  The hash is stored in the database so that we can
           verify later on whether nobody has messed with the store. */
        HashResult hash;
        PathSet references = scanForReferences(actualPath, allPaths, hash);

        if (buildMode == bmCheck) {
            if (!store->isValidPath(path)) continue;
            ValidPathInfo info = worker.store.queryPathInfo(path);
            if (hash.first != info.hash) {
                if (settings.keepFailed) {
                    Path dst = path + checkSuffix;
                    if (pathExists(dst)) deletePath(dst);
                    if (rename(actualPath.c_str(), dst.c_str()))
                        throw SysError(std::format("renaming `{}' to `{}'", actualPath, dst));
                    throw Error(std::format("derivation `{}' may not be deterministic: output `{}' differs from `{}'",
                        drvPath, path, dst));
                } else
                    throw Error(std::format("derivation `{}' may not be deterministic: output `{}' differs",
                        drvPath, path));
            }

            if (settings.printBuildTrace)
                printMsg(lvlError, std::format("@ build-succeeded {} -", drvPath));

            continue;
        }

        /* For debugging, print out the referenced and unreferenced
           paths. */
        for (auto& i : inputPaths) {
            PathSet::iterator j = references.find(i);
            if (j == references.end())
                debug(std::format("unreferenced input: `{}'", i));
            else
                debug(std::format("referenced input: `{}'", i));
        }

        /* Enforce `allowedReferences' and friends. */
        auto checkRefs = [&](const string & attrName, bool allowed, bool recursive) {
            if (drv.env.find(attrName) == drv.env.end()) return;

            PathSet spec = parseReferenceSpecifiers(drv, get(drv.env, attrName));

            PathSet used;
            if (recursive) {
                /* Our requisites are the union of the closures of our references. */
                for (auto & i : references)
                    /* Don't call computeFSClosure on ourselves. */
                    if (actualPath != i)
                        computeFSClosure(worker.store, i, used);
            } else
                used = references;

            for (auto & i : used)
                if (allowed) {
                    if (spec.find(i) == spec.end())
                        throw BuildError(std::format("output (`{}') is not allowed to refer to path `{}'", actualPath, i));
                } else {
                    if (spec.find(i) != spec.end())
                        throw BuildError(std::format("output (`{}') is not allowed to refer to path `{}'", actualPath, i));
                }
        };

        checkRefs("allowedReferences", true, false);
        checkRefs("allowedRequisites", true, true);
        checkRefs("disallowedReferences", false, false);
        checkRefs("disallowedRequisites", false, true);

        if (curRound == nrRounds) {
            worker.store.optimisePath(path); // FIXME: combine with scanForReferences()

            worker.store.markContentsGood(path);
        }

        // goonix: resolved-addressed outputs still produce a signed fact.
        if (resolvedDerivation(drv))
            recordGoonixOutput(worker.store, originalDrvPath, drvPath,
                i.first, path, "local");

        ValidPathInfo info;
        info.path = path;
        info.hash = hash.first;
        info.narSize = hash.second;
        info.references = references;
        info.deriver = drvPath;
        infos.push_back(info);
    }

    /* Compare the result with the previous round, and report which
       path is different, if any.*/
    if (curRound > 1 && prevInfos != infos) {
        assert(prevInfos.size() == infos.size());
        for (auto i = prevInfos.begin(), j = infos.begin(); i != prevInfos.end(); ++i, ++j)
            if (!(*i == *j)) {
                Path prev = i->path + checkSuffix;
                if (pathExists(prev))
                    throw NotDeterministic(
                        std::format("output `{}' of `{}' differs from `{}' from previous round",
                            i->path, drvPath, prev));
                else
                    throw NotDeterministic(
                        std::format("output `{}' of `{}' differs from previous round",
                            i->path, drvPath));
            }
        assert(false); // shouldn't happen
    }

    /* Rename the built package when doing multiple rounds, but
       don't move the package when running in check mode, since
       that would remove the valid package from the store. */
    if (settings.keepFailed && buildMode != bmCheck) {
        for (auto & i : drv.outputs) {
            Path prev = i.second.path + checkSuffix;
            if (pathExists(prev)) deletePath(prev);
            if (curRound < nrRounds) {
                Path dst = i.second.path + checkSuffix;
                if (rename(i.second.path.c_str(), dst.c_str()))
                    throw SysError(std::format("renaming `{}' to `{}'", i.second.path, dst));
            }
        }

    }

    if (curRound < nrRounds) {
        prevInfos = infos;
        discardGoonixOutputs(); // goonix: discard unregistered staged outputs between rounds
        return;
    }

    /* Register each output path as valid, and register the sets of
       paths referenced by each of them.  If there are cycles in the
       outputs, this will fail. */
    try { // goonix: commit facts only after every output is registered
        worker.store.registerValidPaths(infos);
        if (resolvedDerivation(drv))
            recordGoonixFacts(worker.store, originalDrvPath, drvPath, goonixFacts);
    } catch (...) {
        discardGoonixOutputs();
        throw;
    }
}
