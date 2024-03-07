{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative (many, some, (<**>), (<|>))
import Control.Monad (unless)
import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as Parser
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.Char as Char
import Data.String (fromString)
import qualified Options.Applicative as Options
import Options.Applicative.Help.Pretty (Doc, align, fillSep, (.$.))
import qualified System.Directory as Directory
import System.Exit (exitFailure)
import System.IO (IOMode (..), hClose, hGetContents, hPutStr, hPutStrLn, stderr, withFile)
import System.IO.Temp (withSystemTempFile)
import System.Process (callProcess)

data Config
  = Config [Input] [String]
  deriving (Eq, Show)

newtype Input = Input {inputPath :: FilePath}
  deriving (Eq, Show)

configParser :: Options.Parser Config
configParser =
  Config
    <$> many
      ( fmap Input . Options.strOption $
          Options.short 'i'
            <> Options.long "input"
            <> Options.metavar "PATH"
            <> Options.help "Provide the contents at PATH as a temporary input to COMMAND. Can be repeated to pass multiple inputs."
      )
    <*> some (Options.strArgument $ Options.metavar "COMMAND")

paragraph :: (Foldable t) => t String -> Doc
paragraph = fillSep . fmap fromString . foldMap words

main :: IO ()
main = do
  Config inputs command <-
    Options.customExecParser
      (Options.defaultPrefs{Options.prefColumns = 80})
      ( Options.info
          (configParser <**> Options.helper)
          ( Options.fullDesc
              <> Options.progDesc "Pass temporary data to a process"
              <> Options.footerDoc
                ( Just $
                    "Passes data to a process via temporary files."
                      .$. ""
                      .$. "Examples:"
                      .$. ""
                      .$. ( "* "
                              <> align
                                ( paragraph
                                    [ "`tmply -- COMMAND`"
                                    , " - "
                                    , "Calling `tmply` with 0 inputs is equivalent to calling COMMAND directly."
                                    ]
                                )
                          )
                      .$. ""
                      .$. ( "* "
                              <> align
                                ( paragraph
                                    [ "`tmply -i a.txt -i b.txt -- cat {0} {1}`"
                                    , " - "
                                    , "Prints the contents of a.txt and b.txt, via temporary files."
                                    ]
                                )
                          )
                      .$. ""
                      .$. ( "* "
                              <> align
                                ( paragraph
                                    [ "`tmply -i <(getPrivateKey) -- ssh -i {0} user@example.com`"
                                    , " - "
                                    , "Start an SSH connection using the private key that `getPrivateKey` writes to its stdout."
                                    , "The private key is stored in a temporary file while `ssh` is running."
                                    ]
                                )
                          )
                      .$. ""
                      .$. paragraph
                        [ "Data is made available to COMMAND using -i/--input."
                        , "For each PATH provided by -i, a temporary file is created with the contents of PATH."
                        , "These files are deleted when COMMAND terminates[^1]."
                        ]
                      .$. ""
                      .$. paragraph
                        [ "The temporary file paths can be referenced in COMMAND using {n}, where n is a number."
                        , "Each occurrence of {n} is replaced by the nth temporary file."
                        ]
                      .$. ""
                      .$. "Footnotes:"
                      .$. ""
                      .$. ( "* "
                              <> align
                                ( paragraph
                                    [ "[^1]: The temporary files will be deleted when COMMAND terminates, and under normal process termination signals for `tmply`, e.g. SIGTERM, SIGINT, etc."
                                    , "If `tmply` receives a SIGKILL, the temporary files will be leaked."
                                    ]
                                )
                          )
                )
          )
      )
  withInputs inputs $ \files -> do
    let command' = fmap (substitute files) command
    callProcess (head command') (tail command')

withInputs :: [Input] -> ([FilePath] -> IO a) -> IO a
withInputs [] f = f []
withInputs (input : inputs) f = do
  exists <- Directory.doesFileExist (inputPath input)
  unless exists $ do
    hPutStrLn stderr $ "error: " <> inputPath input <> " not found"
    exitFailure

  withSystemTempFile "tmply" $ \path handle -> do
    withFile (inputPath input) ReadMode $ \inputHandle -> do
      hPutStr handle =<< hGetContents inputHandle
      hClose handle
    withInputs inputs $ \paths ->
      f (path : paths)

data Part
  = String String
  | Placeholder Int

substitute :: [FilePath] -> String -> String
substitute files arg =
  case Parser.parseOnly partsParser $ ByteString.Char8.pack arg of
    Left{} ->
      undefined
    Right parts ->
      parts >>= substitutePart
 where
  partsParser = many partParser

  partParser :: Parser Part
  partParser =
    Placeholder <$ Parser.char '{' <*> Parser.decimal <* Parser.char '}'
      <|> String . ByteString.Char8.unpack <$> Parser.takeWhile1 Char.isAscii

  substitutePart :: Part -> String
  substitutePart part =
    case part of
      String s -> s
      Placeholder n -> files !! n