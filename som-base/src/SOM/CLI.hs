module SOM.CLI (Options (..), handlers, outputOrExtension, parser) where

import SOM.Prelude

import Control.Applicative (optional)

import Data.Maybe (fromMaybe)

import Options.Applicative
  ( Parser
  , argument
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
  )

data Options = Options { input ∷ FilePath
                       , output ∷ Maybe FilePath
                       }

parser ∷ Parser Options
parser = Options <$> input <*> output
  where input = argument str (metavar "[INPUT FILE]")
        output = optional (strOption (metavar "[OUTPUT FILE]" <> short 'o' <> long "output"))

handlers ∷ [Handler IO ()]
handlers = [ Handler (\ (StringException e _) → putStrLn e)
           , Handler (\ (e ∷ IOException) → print e)
           ]

outputOrExtension ∷ String → Options → FilePath
outputOrExtension e o = fromMaybe (replaceExtension o.input e) o.output
