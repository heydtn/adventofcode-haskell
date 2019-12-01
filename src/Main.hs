import System.Process
import System.Directory
import System.Environment

import Data.List

import Network.Wreq
import Control.Lens
import Network.HTTP.Client
import Data.Time.Clock
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL

getInputForProblem :: String -> String -> String -> IO String
getInputForProblem year day session = do
    now <- getCurrentTime

    let expires = addUTCTime (30 * 1440) now
        cookieDef = Cookie
            { cookie_name = "session"
            , cookie_value = B.pack session
            , cookie_expiry_time = expires
            , cookie_domain = "adventofcode.com"
            , cookie_path = "/"
            , cookie_creation_time = now
            , cookie_last_access_time = now
            , cookie_persistent = True
            , cookie_host_only = True
            , cookie_secure_only = False
            , cookie_http_only = False
            }
        jar = createCookieJar [ cookieDef ]
        opts = defaults & cookies .~ (Just jar)
    
    let requestURL =
            "https://adventofcode.com/"
            <> year
            <> "/day/"
            <> day
            <> "/input"

    res <- getWith opts requestURL

    return $ case res ^? Network.Wreq.responseBody of
                 Just a -> BL.unpack a
                 Nothing -> error "Unable to fetch input"

createInputFile :: String -> String -> String -> IO ()
createInputFile year day session = do
    input <- getInputForProblem year day session

    let inputFileName = "day" <> day

    let path =
            intercalate "/"
                [ year
                , inputFileName
                , "src"
                , inputFileName <> ".input"
                ]

    let inputLocation = "./" <> path
    writeFile inputLocation input

    return ()

maybeInitializeYear :: String -> IO ()
maybeInitializeYear year =
    createDirectoryIfMissing False ("./" <> year)

initializeProject :: String -> String -> IO ()
initializeProject year day = do
    readCreateProcess processDef ""
    return ()

    where
        projectName = "day" <> day
        templateFile = "aoc_year.hsfiles"

        command = proc "stack" ["new", projectName, templateFile]
        processDef = command { cwd = Just $ "./" <> year }

copyTemplate :: String -> IO ()
copyTemplate year =
    copyFile from to
    where from = "./aoc_year.hsfiles"
          to = year <> "/aoc_year.hsfiles"

removeTemplate :: String -> IO ()
removeTemplate year =
    removeFile $ "./" <> year <> "/aoc_year.hsfiles"

cleanup :: String -> IO ()
cleanup year = do
    removeTemplate year
    return ()

main :: IO ()
main = do
    dayNumber <- fmap read getLine :: IO Integer

    let day = show dayNumber
    let year = "2019"
    maybeInitializeYear year
    copyTemplate year
    initializeProject year day

    sessionId <- getEnv "ADVENT_OF_CODE_SESSION"
    createInputFile year day sessionId

    cleanup year
