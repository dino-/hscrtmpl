#! /usr/bin/env stack
{- stack runghc -}

-- If you need a specific resolver, do this:
-- stack --snapshot lts-22.6 runghc

{- No stack? Use this #! instead of the two lines above:
#! /usr/bin/env runhaskell
-}

{-
  This is a 'skeleton file' for writing shell scripts with Haskell

  Haskell is my go-to language for anything more complicated than a simple
  sequence of shell commands in a bash script. Doing list processing and a lot
  of logic in bash is a grind, to me.

  The idea here is to take a copy of this script and hack it to do what you
  need quickly. Throw the rest out.

  The script starts off (after the imports) with a main full of examples of
  common things along with their bash counterparts.

  After that are a few functions that simplify things like getting the date as
  a String, logging a date-stamped String to stdout and manupulating an
  ExitCode as a true/false value.


  Dino Morelli <dino@ui3.info>
  https://github.com/dino-/hscrtmpl
  version: 2.1  2025-07-15
-}

import Control.Monad
import Data.List
import Data.Maybe
import Data.Time
import Data.Time.Format (defaultTimeLocale)
-- Or use time-locale-compat for backwards compatibility with GHC < 7.10
--import Data.Time.Locale.Compat (defaultTimeLocale)
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.Process
import Text.Printf
--import Text.Regex


main :: IO ()
main = do
  putStrLn "This is a shell script"

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
  e <- doesFileExist "hscrtmpl.hs"          -- [ -f hscrtmpl.hs ]
  when e $ putStrLn "This script exists!"   --    && echo "This script exists!"
   

  -- environment variables (System.Environment)

  -- This will exit with a message and failure code if the variable doesn't exist!
  putStrLn =<< getEnv "SHELL"               -- echo $SHELL

  -- To handle unset variables, use lookupEnv :: String -> IO (Maybe String)

  -- someValue <- maybe                        someValue="${SOMEVAR:?Error: SOMEVAR not set}"
  --    (die "Error: SOMEVAR is not set")
  --    pure =<< lookupEnv "SOMEVAR"

  someValue <- fromMaybe "default value"    -- someValue="${SOMEVAR:-default value}"
     <$> lookupEnv "SOMEVAR"
  putStrLn someValue


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
{- HLINT ignore "Avoid reverse" -}
sequenceWorstEC :: [IO ExitCode] -> IO ExitCode
sequenceWorstEC as =
  fromMaybe ExitSuccess . listToMaybe . reverse . sort <$> sequence as


{- This is similar to the for/in/do/done construct in bash with an important
   difference. This action evaluates to the "worst" exit code of all shell
   commands that are passed after they are evaluated.
-}
systemWorstEC :: [String] -> IO ExitCode
systemWorstEC = sequenceWorstEC . map system
