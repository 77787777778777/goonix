// goonix: build scheduler body moved out of the upstream file.
void DerivationGoal::addInputWaitees()
{ // goonix: schedule an unresolved derivation's inputs
    for (auto & input : drv.inputDrvs)
        addWaitee(worker.makeDerivationGoal(input.first, input.second,
            buildMode == bmRepair ? bmRepair : bmNormal));
    for (auto & source : drv.inputSrcs)
        addWaitee(worker.makeSubstitutionGoal(source));
}

void DerivationGoal::outputsSubstituted()
{
    trace("all outputs substituted (maybe)");

    if (nrFailed > 0 && nrFailed > nrNoSubstituters + nrIncompleteClosure && !settings.tryFallback)
        throw Error(std::format("some substitutes for the outputs of derivation `{}' failed (usually happens due to networking issues); try `--fallback' to build derivation from source ", drvPath));

    /*  If the substitutes form an incomplete closure, then we should
        build the dependencies of this derivation, but after that, we
        can still use the substitutes for this derivation itself. */
    if (nrIncompleteClosure > 0 && !retrySubstitution) retrySubstitution = true;

    nrFailed = nrNoSubstituters = nrIncompleteClosure = 0;

    if (needRestart) {
        needRestart = false;
        haveDerivation();
        return;
    }

    if (goonixAddressed(drv) && !resolvedDerivation(drv)) {
        // goonix: placeholders bypass substitution; realise their inputs first.
        wantedOutputs = PathSet();
        addInputWaitees(); // goonix: same input goals as an ordinary build
        if (waitees.empty()) inputsRealised();
        else state = &DerivationGoal::inputsRealised;
        return;
    }

    unsigned int nrInvalid = checkPathValidity(false, buildMode == bmRepair).size();
    if (buildMode == bmNormal && nrInvalid == 0) {
        done(BuildResult::Substituted);
        return;
    }
    if (buildMode == bmRepair && nrInvalid == 0) {
        repairClosure();
        return;
    }
    if (buildMode == bmCheck && nrInvalid > 0)
        throw Error(std::format("`{}' is missing outputs; build it normally before using `--check'", drvPath));

    /* Otherwise, at least one of the output paths could not be
       produced using a substitute.  So we have to build instead. */

    /* Make sure checkPathValidity() from now on checks all
       outputs. */
    wantedOutputs = PathSet();

    /* The inputs must be built before we can build this goal. */
    for (auto& i : drv.inputDrvs)
        addWaitee(worker.makeDerivationGoal(i.first, i.second, buildMode == bmRepair ? bmRepair : bmNormal));

    for (auto& i : drv.inputSrcs)
        addWaitee(worker.makeSubstitutionGoal(i));

    if (waitees.empty()) /* to prevent hang (no wake-up event) */
        inputsRealised();
    else
        state = &DerivationGoal::inputsRealised;
}
