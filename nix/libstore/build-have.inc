// goonix: build scheduler body moved out of the upstream file.
void DerivationGoal::haveDerivation()
{
    trace("loading derivation");

    if (nrFailed != 0) {
        printMsg(lvlError, std::format("cannot build missing derivation `{}'", drvPath));
        done(BuildResult::MiscFailure);
        return;
    }

    /* `drvPath' should already be a root, but let's be on the safe
       side: if the user forgot to make it a root, we wouldn't want
       things being garbage collected while we're busy. */
    worker.store.addTempRoot(drvPath);

    assert(worker.store.isValidPath(drvPath));

    /* Get the derivation. */
    drv = derivationFromPath(worker.store, drvPath);
    // goonix: prepare resolved inputs and outputs; schedule only their state.
    auto goonixGoal = prepareGoonixGoal(worker.store, drvPath, originalDrvPath,
        drv, originalDrv, buildMode, settings.get("build-repeat", 0));
    originalDrvPath = goonixGoal.originalPath;
    if (goonixGoal.action == GoonixGoalAction::ordinary) {
        checkInputAddressedPolicy(worker.store, drv); // goonix: mixed policy
    } else {
        if (originalDrv.outputs.empty()) originalDrv = drv;
        drv = goonixGoal.drv;
        inputPaths = goonixGoal.inputPaths; allPaths = goonixGoal.allPaths;
        missingPaths = goonixGoal.missingPaths;
        fixedOutput = goonixGoal.fixedOutput; nrRounds = goonixGoal.rounds;
        if (goonixGoal.action == GoonixGoalAction::done)
            done(BuildResult::AlreadyValid);
        else {
            state = goonixGoal.action == GoonixGoalAction::build
                ? &DerivationGoal::tryToBuild : &DerivationGoal::outputsSubstituted;
            worker.wakeUp(shared_from_this());
        }
        return;
    }


    for (auto& i : drv.outputs)
        worker.store.addTempRoot(i.second.path);

    /* Check what outputs paths are not already valid. */
    PathSet invalidOutputs = checkPathValidity(false, buildMode == bmRepair);

    /* If they are all valid, then we're done. */
    if (invalidOutputs.size() == 0 && buildMode == bmNormal) {
        done(BuildResult::AlreadyValid);
        return;
    }

    /* Check whether any output previously failed to build.  If so,
       don't bother. */
    for (auto& i : invalidOutputs)
        if (pathFailed(i)) return;

    /* We are first going to try to create the invalid output paths
       through substitutes.  If that doesn't work, we'll build
       them. */
    if (settings.useSubstitutes && substitutesAllowed(drv))
        for (auto& i : invalidOutputs)
            addWaitee(worker.makeSubstitutionGoal(i, buildMode == bmRepair));

    if (waitees.empty()) /* to prevent hang (no wake-up event) */
        outputsSubstituted();
    else
        state = &DerivationGoal::outputsSubstituted;
}
