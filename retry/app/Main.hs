module Main where

import Control.Applicative (some, (<**>))
import Control.Concurrent (threadDelay)
import qualified Options.Applicative as Options
import System.Exit (ExitCode (..), exitWith)
import System.IO (hPutStrLn, stderr)
import System.Process (CreateProcess (..), proc, waitForProcess, withCreateProcess)

data Config = Config {numRetries :: Int, retryDelay :: Int, command :: [String]}

configParser :: Options.Parser Config
configParser =
  Config
    <$> Options.option
      Options.auto
      ( Options.short 'n'
          <> Options.long "num-retries"
          <> Options.metavar "COUNT"
          <> Options.value 10
          <> Options.showDefault
          <> Options.help "Maximum number of retries"
      )
    <*> Options.option
      Options.auto
      ( Options.short 'd'
          <> Options.long "delay"
          <> Options.metavar "TIME"
          <> Options.value 3
          <> Options.showDefault
          <> Options.help "Seconds to wait before retrying"
      )
    <*> some (Options.strArgument $ Options.metavar "COMMAND")

callProcessWithExitCode :: FilePath -> [String] -> IO ExitCode
callProcessWithExitCode cmd args = do
  withCreateProcess (proc cmd args){delegate_ctlc = True} $ \_ _ _ -> waitForProcess

main :: IO ()
main = do
  config <-
    Options.execParser $
      Options.info
        (configParser <**> Options.helper)
        Options.fullDesc

  let
    loop :: Int -> Int -> IO ()
    loop retryNum remainingAttempts
      | remainingAttempts < 1 = undefined
      | otherwise = do
          exitCode <- callProcessWithExitCode (head $ command config) (tail $ command config)
          case exitCode of
            ExitSuccess ->
              pure ()
            ExitFailure{} ->
              if remainingAttempts == 1
                then exitWith exitCode
                else do
                  let retryNum' = retryNum + 1
                  hPutStrLn stderr $
                    "retry: retrying in "
                      <> show (retryDelay config)
                      <> " seconds ("
                      <> show retryNum'
                      <> "/"
                      <> show (numRetries config)
                      <> ")"
                  threadDelay (retryDelay config * 1000000)
                  loop retryNum' (remainingAttempts - 1)

  loop 0 $ numRetries config + 1
