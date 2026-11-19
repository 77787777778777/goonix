// goonix: member hooks for the CA scheduler; no upstream function bodies.
bool DerivationGoal::goonixHaveDerivation()
{
    if (!goonixAddressed(drv)) {
        checkInputAddressedPolicy(worker.store, drv);
        return false;
    }
    auto prepared = prepareGoonixGoal(worker.store, drvPath, originalDrvPath,
        drv, originalDrv, buildMode, settings.get("build-repeat", 0));
    originalDrvPath = prepared.originalPath;
    if (originalDrv.outputs.empty()) originalDrv = drv;
    drv = prepared.drv;
    if (prepared.action == GoonixGoalAction::done) {
        done(BuildResult::AlreadyValid);
        return true;
    }
    inputPaths = prepared.inputPaths;
    allPaths = prepared.allPaths;
    missingPaths = prepared.missingPaths;
    fixedOutput = prepared.fixedOutput;
    nrRounds = prepared.rounds;
    state = prepared.action == GoonixGoalAction::build
        ? &DerivationGoal::tryToBuild : &DerivationGoal::outputsSubstituted;
    worker.wakeUp(shared_from_this());
    return true;
}

bool DerivationGoal::goonixRealiseInputsFirst()
{
    if (!goonixAddressed(drv) || resolvedDerivation(drv)) return false;
    wantedOutputs = PathSet();
    addInputWaitees();
    if (waitees.empty()) inputsRealised();
    else state = &DerivationGoal::inputsRealised;
    return true;
}

bool DerivationGoal::goonixResolveInputs()
{
    if (!goonixAddressed(drv) || resolvedDerivation(drv)) return false;
    auto resolved = resolveGoonixBuild(worker.store, originalDrvPath, originalDrv);
    drv = resolved.drv;
    drvPath = resolved.resolvedPath;
    haveDerivation();
    if (state == &DerivationGoal::tryToBuild) worker.wakeUp(shared_from_this());
    return true;
}

void DerivationGoal::addInputWaitees()
{
    for (auto & input : drv.inputDrvs)
        addWaitee(worker.makeDerivationGoal(input.first, input.second,
            buildMode == bmRepair ? bmRepair : bmNormal));
    for (auto & source : drv.inputSrcs)
        addWaitee(worker.makeSubstitutionGoal(source));
}

void DerivationGoal::goonixRegisterOutputs(InodesSeen & inodesSeen,
    ValidPathInfos & infos)
{
    goonixOutputResult = finaliseGoonixOutputs(worker.store, drv, originalDrv,
        drvPath, originalDrvPath, allPaths, missingPaths, useChroot, chrootRootDir,
        buildMode, buildUser.enabled() ? buildUser.getUID() : (uid_t) -1,
        buildUser.enabled() ? buildUser.getGID() : (gid_t) -1, inodesSeen,
        [&] { if (buildUser.enabled()) buildUser.kill(); });
    infos.splice(infos.end(), goonixOutputResult.infos);
}

void DerivationGoal::goonixRecordResolvedOutput(const string & output,
    const Path & path)
{
    if (resolvedDerivation(drv))
        recordGoonixOutput(worker.store, originalDrvPath, drvPath,
            output, path, "local");
}

void DerivationGoal::goonixDiscardOutputs()
{
    discardGoonixOutputs(worker.store, goonixOutputResult);
}

void DerivationGoal::goonixRegisterValidPaths(ValidPathInfos & infos)
{
    try {
        worker.store.registerValidPaths(infos);
        if (resolvedDerivation(drv))
            recordGoonixFacts(worker.store, originalDrvPath, drvPath,
                goonixOutputResult.facts);
    } catch (...) {
        goonixDiscardOutputs();
        throw;
    }
}

bool DerivationGoal::goonixRecheckRealised()
{
    if (buildMode != bmNormal ||
        !goonixBuildAlreadyRealised(worker.store, originalDrvPath, drv)) return false;
    outputLocks.setDeletion(true);
    outputLocks.unlock();
    done(BuildResult::AlreadyValid);
    return true;
}
