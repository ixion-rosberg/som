module SOM.CLI (Options (..), fromEither, fromMaybe, handlers, outputOrExtension, parseOptions, parser, runCLI) where

import SOM.Prelude

import Control.Applicative (optional)
import Control.Monad.IO.Class (MonadIO)

import Data.Either.Extra (mapLeft, maybeToEither)
import Data.Maybe qualified as M (fromMaybe)

import Options.Applicative
  ( Parser
  , argument
  , execParser
  , fullDesc
  , info
  , long
  , metavar
  , short
  , str
  , strOption
  )

import System.FilePath (replaceExtension)

import UnliftIO.Exception
  ( Handler (..)
  , IOException
  , StringException (..)
  , catches
  , stringException
  )
import UnliftIO.Exception qualified as U (fromEither)

data Options = Options { input ∷ FilePath
                       , output ∷ Maybe FilePath
                       }

parser ∷ Parser Options
parser = Options <$> input <*> output
  where input = argument str (metavar "[INPUT FILE]")
        output = optional (strOption (metavar "[OUTPUT FILE]" <> short 'o' <> long "output"))

handlers ∷ [Handler IO ()]
handlers = [ Handler (\ (StringException e _) → putStrLn e)
           , Handler (\ (e ∷ IOException)     → print e)
           ]

outputOrExtension ∷ String → Options → FilePath
outputOrExtension e o = M.fromMaybe (replaceExtension o.input e) o.output

fromEither ∷ MonadIO μ ⇒ Either String α → μ α
fromEither = U.fromEither ∘ mapLeft stringException

fromMaybe ∷ MonadIO μ ⇒ String → Maybe α → μ α
fromMaybe e = fromEither ∘ maybeToEither e

parseOptions ∷ IO Options
parseOptions = execParser (info parser fullDesc)

runCLI ∷ IO () → IO ()
runCLI = (flip catches) handlers
