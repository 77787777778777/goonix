{-# LANGUAGE GHC2024 #-}

module Main (main) where

import Control.Monad (forM_)
import Data.Version (makeVersion, showVersion)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

version :: String
version = "0.1.0.0"

usage :: String
usage = unlines
    [ "Usage: guix-daemon [OPTION]..."
    , "Perform derivation builds and store accesses."
    , ""
    , "  -c, --cores=N              use N CPU cores to build each derivation"
    , "  -M, --max-jobs=N           allow at most N build jobs"
    , "      --timeout=SECONDS      mark builds as failed after SECONDS"
    , "      --max-silent-time=SECONDS"
    , "                               mark silent builds as failed after SECONDS"
    , "      --disable-chroot       disable chroot builds"
    , "      --chroot-directory=DIR add DIR to the build chroot"
    , "      --build-users-group=GROUP"
    , "                               perform builds as a user of GROUP"
    , "      --no-substitutes       do not use substitutes"
    , "      --substitute-urls=URLS use URLS as substitute providers"
    , "      --no-offload           do not attempt to offload builds"
    , "      --cache-failures       cache build failures"
    , "      --rounds=N             build each derivation N times in a row"
    , "      --lose-logs            do not keep build logs"
    , "      --log-compression=TYPE use none, gzip or bzip2 for build logs"
    , "      --discover[=yes/no]    discover local substitute servers"
    , "      --disable-deduplication"
    , "                               disable automatic file deduplication"
    , "      --impersonate-linux-2.6 impersonate Linux 2.6"
    , "      --gc-keep-outputs[=yes/no]"
    , "                               keep outputs of live derivations"
    , "      --gc-keep-derivations[=yes/no]"
    , "                               keep derivations for live outputs"
    , "      --listen=SOCKET        listen for connections on SOCKET"
    , "      --debug                produce debugging output"
    , "      --isolate-host-loopback"
    , "                               isolate fixed-output builds from host loopback"
    , "      --allow-aslr           allow address space layout randomization"
    , "      --help                 display this help and exit"
    , "      --version              display version information and exit"
    ]

main :: IO ()
main = do
    args <- getArgs
    if args == ["--version"] || args == ["-V"]
        then putStrLn ("guix-daemon (GNU Guix) " ++ showVersion (Data.Version.makeVersion [0,1,0,0]))
        else if args == ["--help"] || args == ["--usage"]
            then putStr usage
            else do
                hPutStrLn stderr "guix-daemon: daemon server is not available in this build"
                forM_ args (hPutStrLn stderr)
                exitFailure
