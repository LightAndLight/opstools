{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<**>))
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Data.Bits ((.|.))
import Data.Foldable (traverse_)
import GHC.IO.StdHandles (withFileBlocking)
import qualified Options.Applicative as Options
import System.Exit (ExitCode (..), exitWith)
import System.FilePath ((</>))
import System.IO (IOMode (..), hPutStrLn)
import qualified System.IO
import System.IO.Temp (withSystemTempDirectory)
import System.Posix
import System.Process (readProcessWithExitCode)

data Config
  = Private
  | Public
  deriving (Eq, Show)

configParser :: Options.Parser Config
configParser =
  Options.hsubparser
    ( Options.command
        "private"
        ( Options.info
            configPrivateParser
            ( Options.fullDesc
                <> Options.progDesc "Generate a private key"
                <> Options.footer "Generates a new OpenSSH format private key and writes it to stdout."
            )
        )
        <> Options.command
          "public"
          ( Options.info
              configPublicParser
              ( Options.fullDesc
                  <> Options.progDesc "Generate a public key from a private key"
                  <> Options.footer "Reads an OpenSSH format public key from stdin and writes its public key to stdout."
              )
          )
    )

configPrivateParser :: Options.Parser Config
configPrivateParser =
  pure Private

configPublicParser :: Options.Parser Config
configPublicParser =
  pure Public

main :: IO ()
main = do
  config <-
    Options.execParser
      ( Options.info
          (configParser <**> Options.helper)
          ( Options.fullDesc
              <> Options.progDesc "Generate OpenSSH format private and public keys"
          )
      )
  case config of
    Private -> genPrivateKey
    Public -> genPublicKey

genPrivateKey :: IO ()
genPrivateKey = do
  (exitCode, stdoutString, stderrString) <- do
    withSystemTempDirectory "genssh-private" $ \dir -> do
      let key = dir </> "key"
      createNamedPipe key (ownerReadMode .|. ownerWriteMode)

      let keyPub = dir </> "key.pub"
      createNamedPipe keyPub (ownerReadMode .|. ownerWriteMode)

      keyString <- readFile key
      _keyPubString <- readFile keyPub

      result <- readProcessWithExitCode "ssh-keygen" ["-t", "ed25519", "-N", "", "-f", key] "y\n"
      putStr keyString
      pure result

  case exitCode of
    ExitFailure code -> do
      hPutStrLn System.IO.stderr $ "error: ssh-keygen failed (exit code " <> show code <> ")\n"
      hPutStrLn System.IO.stderr "stdout:"
      traverse_ (hPutStrLn System.IO.stderr . ("  " <>)) $ lines stdoutString
      hPutStrLn System.IO.stderr "stderr:"
      traverse_ (hPutStrLn System.IO.stderr . ("  " <>)) $ lines stderrString
    ExitSuccess -> pure ()
  exitWith exitCode

genPublicKey :: IO ()
genPublicKey = do
  (exitCode, stdoutString, stderrString) <-
    withSystemTempDirectory "genssh-public" $ \dir -> do
      let key = dir </> "key"
      createNamedPipe key (ownerReadMode .|. ownerWriteMode)

      result <- newEmptyMVar
      _ <- forkIO $ putMVar result =<< readProcessWithExitCode "ssh-keygen" ["-f", key, "-y"] ""
      {- This was a big WTF. I was getting a `No such device or address` if I tried to write to the
      FIFO too early.

      I think I've stumbled across [this issue from
      2001](https://mail.haskell.org/pipermail/haskell/2001-February/006703.html).
      Named pipes don't work when the writer is opened before the reader.

      After reading a relevant StackOverflow thread (<https://stackoverflow.com/questions/24972911/when-i-try-to-open-a-fifo-o-wronly-i-get-a-no-such-device-or-address-error>),
      my hypothesis was that Haskell opens files in non-blocking mode, causing the error.

      I found
      [`openFileBlocking`](https://hackage.haskell.org/package/base-4.19.1.0/docs/GHC-IO-Handle-FD.html#v:openFileBlocking),
      which comments on this exact use case.
      -}
      withFileBlocking key WriteMode $ \h ->
        System.IO.hPutStr h =<< getContents
      takeMVar result

  putStr stdoutString

  case exitCode of
    ExitFailure code -> do
      hPutStrLn System.IO.stderr $ "error: ssh-keygen failed (exit code " <> show code <> ")\n"
      hPutStrLn System.IO.stderr "stderr:"
      traverse_ (hPutStrLn System.IO.stderr . ("  " <>)) $ lines stderrString
    ExitSuccess -> pure ()
  exitWith exitCode
