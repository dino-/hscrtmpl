#! /usr/bin/env stack
-- stack --snapshot lts-22.6 runghc --package heredoc --package optparse-applicative --package prettyprinter
{-# LANGUAGE QuasiQuotes #-}

-- If you don't need a specific snapshot resolver, do this:
-- stack runghc --package ...

{-
  This is a 'skeleton file' for writing shell scripts with Haskell that have
  sophisticated arg parsing with optparse-applicative.

  The idea here is to take a copy of this script and hack it to do what you
  need quickly. Throw the rest out.

  For more examples of doing common shell tasks, see the hscrtmpl.hs script
  also in this project.


  Dino Morelli <dino@ui3.info>
  https://github.com/dino-/hscrtmpl
  version: 2.1  2025-07-15
-}

import Options.Applicative
import Prettyprinter (pretty)
import System.Environment (getProgName)
import Text.Heredoc (here)
import Text.Printf (printf)


main :: IO ()
main = do
  opts <- parseOpts
  print opts


data Verbosity = Normal | Verbose
  deriving Show  -- For debugging, possibly not needed

data Options = Options
  { optSwitch :: Bool
  , optMaybeString :: Maybe String
  , optFlag :: Verbosity
  , optString :: String
  , optArgument :: String
  , optArguments :: [String]
  }
  deriving Show  -- For debugging, possibly not needed


parseOpts :: IO Options
parseOpts = do
  pn <- getProgName
  execParser $ info (parser <**> helper)
    (  header (printf "%s - Argument parsing demo script" pn)
    <> footer'
    )


-- For more help on building parsers: https://github.com/pcapriotti/optparse-applicative
{- HLINT ignore "Move brackets to avoid $" -}
parser :: Parser Options
parser = Options
  <$> switch
        (  long "switch" <> short 's'
        <> help "A switch option"
        )
  <*> ( optional $ strOption
        (  long "maybe-string" <> short 'm' <> metavar "STRING"
        <> help "An optional string option"
        )
      )
  <*> flag Normal Verbose
        (  long "verbose" <> short 'v'
        <> help "A flag option"
        )
  <*> strOption
        (  long "string" <> short 'S' <> metavar "STRING"
        <> help "A required string option"
        )
  <*> argument str
        (  metavar "DIR"
        <> help "A required single argument"
        )
  <*> some ( argument str
        (  metavar "FILES..."
        <> help "Multiple arguments, one or more required"
        )
      )


footer' :: InfoMod a
footer' = footerDoc . Just . pretty $ content
  where content = [here|OVERVIEW

Put more info here about what this script is for and how it works

v2.1  2025-07-15  Dino Morelli <dino@ui3.info>|]
