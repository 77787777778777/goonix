#include "store-api.hh"
#include "globals.hh"
#include <iostream>
int main() {
    nix::settings.nixStore = "/gnu/store";
    nix::Hash h = nix::parseHash(nix::htSHA256, "00b55e2a318800d887e847f3466c4774b7e5a8d441ddc5c3a3549d0169ef8523");
    std::cout << nix::makeOutputPath("out", h, "jansson-2.15.0") << '\n';
    std::cout << nix::makeStorePath("output:out", h, "jansson-2.15.0") << '\n';
    std::cout << nix::printHash32(nix::compressHash(nix::hashString(nix::htSHA256, "output:out:sha256:00b55e2a318800d887e847f3466c4774b7e5a8d441ddc5c3a3549d0169ef8523:/gnu/store:jansson-2.15.0"), 20)) << '\n';
    std::cout << nix::makeStorePath("source", nix::hashString(nix::htSHA256, "hello"), "hello") << '\n';
}
