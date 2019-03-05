module Main where


import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitFailure))

import qualified Text.JSON as TJ

import Data.Depsolver.Parse
    ( parseRepo
    , parseRepoState
    , parseConstraints )
import Data.Depsolver.Solver (solve)


main :: IO ()
main = do
  args <- getArgs
  case args of
    [repoF, initialF, constrsF] -> do
        repo <- readAndParse repoF parseRepo "repository"
        repoState <- readAndParse initialF parseRepoState "initial state"
        constraints <- readAndParse constrsF parseConstraints "constraints"
        let solved = solve repo constraints repoState
        case solved of
          Nothing -> exitErr "couldn't solve constraints"
          Just (_, _, cmds) -> putStrLn $ TJ.encodeStrict cmds
    _ -> exitErr "usage: ./solve REPO INITIAL-STATE CONSTRAINTS"
    where exitErr s = putStrLn s >> exitWith (ExitFailure 1)
          readAndParse f p e = readFile f >>= maybe (exitErr $ "could not parse " <> e) pure . p
