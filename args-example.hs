#! /usr/bin/env stack
-- stack runghc --package ansi-wl-pprint --package heredoc --package optparse-applicative
{-# LANGUAGE QuasiQuotes #-}

-- If you need a specific resolver, do this:
-- stack --resolver lts-16.11 runghc --package ...

{-
  This is a 'skeleton file' for writing shell scripts with Haskell that have
  sophisticated arg parsing with optparse-applicative.

  The idea here is to take a copy of this script and hack it to do what you
  need quickly. Throw the rest out.

  The script starts off (after the imports) with some example arg parsing code
  and then a simple main that shows the results.

  For more examples of doing common shell tasks, see the hscrtmpl.hs script
  also in this project.


  Dino Morelli <dino@ui3.info>
  https://github.com/dino-/hscrtmpl
  version: 2.0  2023-06-27
-}

import Options.Applicative
import System.Environment ( getProgName )
import Text.Heredoc ( here )
import Text.PrettyPrint.ANSI.Leijen ( string )
import Text.Printf ( printf )


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


-- For more help on building parsers: https://github.com/pcapriotti/optparse-applicative
parser :: Parser Options
parser = Options
  <$> ( switch
        (  long "switch" <> short 's'
        <> help "A switch option"
        )
      )
  <*> ( optional $ strOption
        (  long "maybe-string" <> short 'm' <> metavar "STRING"
        <> help "An optional string option"
        )
      )
  <*> ( flag Normal Verbose
        (  long "verbose" <> short 'v'
        <> help "A flag option"
        )
      )
  <*> ( strOption
        (  long "string" <> short 'S' <> metavar "STRING"
        <> help "A required string option"
        )
      )
  <*> ( argument str
        (  metavar "DIR"
        <> help "A required single argument"
        )
      )
  <*> some ( argument str
        (  metavar "FILES..."
        <> help "Multiple arguments, one or more required"
        )
      )


parseOpts :: IO Options
parseOpts = do
  pn <- getProgName
  execParser $ info (parser <**> helper)
    (  header (printf "%s - Argument parsing demo script" pn)
    <> footer'
    )


footer' :: InfoMod a
footer' = footerDoc . Just $ string content
  where content = [here|OVERVIEW

Put more info here about what this script is for an how it works

v2.0  2023-06-27  Dino Morelli <dino@ui3.info>|]


main :: IO ()
main = do
  opts <- parseOpts
  print opts
