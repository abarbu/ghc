module Main (main) where

import Development.Shake

import qualified Base
import qualified Rules
import qualified Rules.Cabal
import qualified Rules.Config
import qualified Rules.Generate
import qualified Rules.IntegerGmp
import qualified Rules.Libffi
import qualified Rules.Oracles
import qualified Rules.Perl

main :: IO ()
main = shakeArgs options rules
  where
    rules = mconcat
        [ Rules.Cabal.cabalRules
        , Rules.Config.configRules
        , Rules.Generate.copyRules
        , Rules.Generate.generateRules
        , Rules.Perl.perlScriptRules
        , Rules.generateTargets
        , Rules.IntegerGmp.integerGmpRules
        , Rules.Libffi.libffiRules
        , Rules.Oracles.oracleRules
        , Rules.packageRules ]
    options = shakeOptions
        { shakeFiles    = Base.shakeFilesPath
        , shakeProgress = progressSimple
        , shakeTimings  = True }
