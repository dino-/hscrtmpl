#! /usr/bin/env stack
-- stack runghc --package ansi-wl-pprint --package heredoc --package optparse-applicative
{-# LANGUAGE QuasiQuotes #-}

-- If you need a specific resolver, do this:
-- stack --resolver lts-7.8 runghc --package ...

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
  version: 1.0  2023-06-27
-}

import Control.Monad
import Data.List
import Data.Maybe
import Data.Time
import Data.Time.Format ( defaultTimeLocale )
import Options.Applicative
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Process
import Text.Heredoc ( here )
import Text.PrettyPrint.ANSI.Leijen ( string )
import Text.Printf
--import Text.Regex


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
        (  long "switch"
        <> short 's'
        <> help "A switch option"
        )
      )
  <*> ( optional $ strOption
        (  long "maybe-string"
        <> short 'm'
        <> metavar "STRING"
        <> help "An optional string option"
        )
      )
  <*> ( flag Normal Verbose
        (  long "verbose"
        <> short 'v'
        <> help "A flag option"
        )
      )
  <*> ( strOption
        (  long "string"
        <> short 'S'
        <> metavar "STRING"
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

v1.0  2023-06-27  Dino Morelli <dino@ui3.info>|]


main :: IO ()
main = do
  putStrLn "This is a shell script"

  opts <- parseOpts
  print opts

                                             -- in bash:
  -- dates (Date.Time)
  putStrLn =<< date                         -- date
  putStrLn =<< dateFormat "%Y%m%d"          -- date +"%Y%m%d"
    -- These two functions below

  -- file/dir things
  --   (System.Directory)
  putStrLn =<< getHomeDirectory             -- echo $HOME
  print =<< doesFileExist "foo"             -- [ -f foo ]
  print =<< doesDirectoryExist "bar"        -- [ -d bar ]
  putStrLn =<< getCurrentDirectory          -- pwd
  --setCurrentDirectory "/tmp"                -- cd /tmp
  print =<< makeAbsolute "some/relative/path"
  --   (System.FilePath)
  let (path, ext) = splitExtension "foo/bar.baz"
  print (path, ext)

  -- conditional statements (Control.Monad)
  e <- doesFileExist "args-example.hs"          -- [ -f hscrtmpl.hs ]
  when e $ putStrLn "This script exists!"   --    && echo "This script exists!"
   

  -- environment variables (System.Environment)
  putStrLn =<< getEnv "SHELL"               -- echo $SHELL

  -- arguments (System.Environment)
  --(arg1 : arg2 : _) <- getArgs              -- arg1=$1 ; arg2=$2

  -- string interpolation (Text.Printf)
  printf "The %s is %d\n"                   -- S="answer" ; D=42
    "answer" (42::Int)                     -- echo "The $S is $D"

  -- sequence expressions
  -- [1..10]                                   -- {1..10}

  -- execution, exit code (System.Process)
  ec <- system "ls -l"                      -- ls -l
  print ec                                  -- echo $?

  -- execution, capture stdout (System.Process)
  output <- readProcess "ls" ["-l"]         -- output=$(ls -l)
    "stdin data, if desired"

    -- or use readProcessWithExitCode
    -- :: FilePath -> [String] -> String -> IO (ExitCode, String, String)
    --    program     args        stdin         exitcode  stdout  stderr

  -- regular expressions (Text.Regex)
  --print $ matchRegex                        -- (see bash docs for =~
  --   (mkRegex "a(.)b(.)") "axby"            --  and BASH_REMATCH)

  -- Handy date-stamping logM function below
  logM "Example of a log message"

  -- exiting (System.Exit)
  exitSuccess                               -- exit 0
  --exitFailure                             -- exit 1
  --exitWith $ ExitFailure 3                -- exit 3


{- Get the current date/time as a string in RFC822 format
   Looks like this in my locale: Mon Feb 13 16:21:38 EST 2012
-}
date :: IO String
date = dateFormat "%c"


{- Get the current date/time as a string in the specified format
   For format string help, see man 3 strftime
-}
dateFormat :: String -> IO String
dateFormat fmt = formatTime defaultTimeLocale fmt <$>
   (getCurrentTime >>= utcToLocalZonedTime)


{- Output a message with datestamp
-}
logM :: String -> IO ()
logM msg = do
   tstamp <- dateFormat "%F %T"
   printf "%s> %s\n" tstamp msg


{- Turn an exit code (say, from system) into a Bool
-}
ok :: ExitCode -> Bool
ok ExitSuccess = True
ok _           = False


{- Turn a Bool into an exit code
-}
toExitCode :: Bool -> ExitCode
toExitCode True  = ExitSuccess
toExitCode False = ExitFailure 1


{- Exit with a success or failure code using a Bool
-}
exitBool :: Bool -> IO ()
exitBool = exitWith . toExitCode


{- This is similar to the for/in/do/done construct in bash with an important
   difference. This action evaluates to the "worst" exit code of all IO actions
   that are passed after they are evaluated.
-}
sequenceWorstEC :: [IO ExitCode] -> IO ExitCode
sequenceWorstEC as =
  fromMaybe ExitSuccess . listToMaybe . reverse . sort <$> sequence as


{- This is similar to the for/in/do/done construct in bash with an important
   difference. This action evaluates to the "worst" exit code of all shell
   commands that are passed after they are evaluated.
-}
systemWorstEC :: [String] -> IO ExitCode
systemWorstEC = sequenceWorstEC . map system
