#include "store-api.hh"
#include "derivations.hh"
#include "globals.hh"
#include <iostream>
int main(int argc, char **argv) {
    nix::settings.nixStore = "/gnu/store";
    auto drv = nix::readDerivation(argv[1]);
    for (auto &i : drv.outputs) { i.second.path = ""; drv.env[i.first] = ""; }
    nix::StoreAPI *unused = nullptr;
    (void) unused;
    std::cout << nix::unparseDerivation(drv) << '\n';
    std::cout << nix::printHash(nix::hashString(nix::htSHA256, nix::unparseDerivation(drv))) << '\n';
    return 0;
}
