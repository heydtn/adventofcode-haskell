{-# LANGUAGE OverloadedStrings #-}

import System.Directory
import System.Environment
import Network.HTTP.Req
import Web.Cookie
import Data.Either (fromRight)
import Data.Text (replace, unpack, pack)

createProblemSource [] = do
    return ()
createProblemSource num = do
    template <- readFile "code.template"
    writeFile 
        (num <> "/day" <> num <> ".hs")
        . unpack
        . replace "{% fileNameHere %}" (pack $ "day" <> num <> ".input")
        . pack
        $ template

createFiles [] = return ()
createFiles "/" = return ()
createFiles num = do
    createDirectoryIfMissing False num
    createProblemSource num

--    session <- getEnv "ADVENT_OF_CODE_SESSION"
--    body <- getResponseBody =<< sendRequest session num
--    writeFile (num <> "/" <> "day" <> num <> ".input") body

--sendRequest session day = do
--    let cookie = defaultSetCookie
--        { setCookieName = "session"
--        , setCookieValue = "cookieValue"
--        }
--
--    simpleHTTP
--    . flip setHeaders [ mkHeader HdrCookie ("session="<>session) ]
--    $ getRequest ("https://adventofcode.com/2018/day/" <> day)

main = do
    num <- getLine
    createFiles num
