// goonix: build scheduler body moved out of the upstream file.
void DerivationGoal::inputsRealised()
{
    trace("all inputs realised");

    if (nrFailed != 0) {
        printMsg(lvlError,
            std::format("cannot build derivation `{}': {} dependencies couldn't be built",
            drvPath, nrFailed));
        done(BuildResult::DependencyFailed);
        return;
    }

    if (goonixAddressed(drv) && !resolvedDerivation(drv)) {
        // goonix: resolve, persist and select, then reschedule the resolved goal.
        auto resolved = resolveGoonixBuild(worker.store, originalDrvPath, originalDrv);
        drv = resolved.drv;
        drvPath = resolved.resolvedPath;
        haveDerivation(); // goonix: schedule the resolved derivation
        if (state == &DerivationGoal::tryToBuild) worker.wakeUp(shared_from_this());
        return;
    }

    if (retrySubstitution) {
        haveDerivation();
        return;
    }

    /* Gather information necessary for computing the closure and/or
       running the build hook. */

    /* The outputs are referenceable paths. */
    for (auto& i : drv.outputs) {
        debug(std::format("building path `{}'", i.second.path));
        allPaths.insert(i.second.path);
    }

    /* Determine the full set of input paths. */

    /* First, the input derivations. */
    for (auto& i : drv.inputDrvs) {
        /* Add the relevant output closures of the input derivation
           `*i' as input paths.  Only add the closures of output paths
           that are specified as inputs. */
        assert(worker.store.isValidPath(i.first));
        Derivation inDrv = derivationFromPath(worker.store, i.first);
        for (auto& j : i.second)
            if (inDrv.outputs.find(j) != inDrv.outputs.end())
                computeFSClosure(worker.store, inDrv.outputs[j].path, inputPaths);
            else
                throw Error(
                    std::format("derivation `{}' requires non-existent output `{}' from input derivation `{}'",
                    drvPath, j, i.first));
    }

    /* Second, the input sources. */
    for (auto& i : drv.inputSrcs)
        computeFSClosure(worker.store, i, inputPaths);

    debug(std::format("added input paths {}", showPaths(inputPaths)));

    allPaths.insert(inputPaths.begin(), inputPaths.end());

    /* goonix: shared build-kind calculation; fixed outputs run one round. */
    auto kind = goonixBuildKind(drv, settings.get("build-repeat", 0));
    fixedOutput = kind.fixedOutput;
    nrRounds = kind.rounds;

    /* Okay, try to build.  Note that here we don't wait for a build
       slot to become available, since we don't need one if there is a
       build hook. */
    state = &DerivationGoal::tryToBuild;
    worker.wakeUp(shared_from_this());
}
