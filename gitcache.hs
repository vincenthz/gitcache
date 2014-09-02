#!/usr/bin/runhaskell
--
-- Simple git overlay that cache repository locally to the user,
-- for faster cloning and updating
--
module Main where

import Data.List
import Control.Applicative
import System.Environment
import System.Directory
import System.FilePath
import System.Process
import System.Exit
import Crypto.Hash
import qualified Data.ByteString.UTF8 as UTF8

urlToHash :: String -> String
urlToHash url = show (hash (UTF8.fromString url) :: Digest SHA1)

urlToName :: String -> String
urlToName s = reverse $ fst $ break (== '/') $ reverse s

rawSystemEC s l = do
    ec <- rawSystem s l
    case ec of
        ExitSuccess   -> return ()
        ExitFailure i -> error ("call: " ++  intercalate " " (s : l) ++ " exit with " ++ show i)

withSetDirectory newDir f = do
    old <- getCurrentDirectory
    setCurrentDirectory newDir
    f
    setCurrentDirectory old

cloneUrl gitCacheDir url pushUrl = do
    let destName = urlToHash url
        destDir  = gitCacheDir </> destName
    clonedAlready <- doesDirectoryExist destDir
    if clonedAlready
        then do
            -- update repository
            withSetDirectory destDir $
                rawSystemEC "git" [ "fetch", "--all" ]
        else
            withSetDirectory gitCacheDir $
                rawSystemEC "git" [ "clone", "--mirror", url, destName ]
    -- and clone locally and replace the origin url
    rawSystemEC "git" [ "clone", destDir, urlToName url ]
    withSetDirectory (urlToName url) $ do
        rawSystemEC "git" [ "remote", "set-url", "origin", url ]
        maybe (return ()) (\purl -> rawSystemEC "git" [ "remote", "set-url", "origin", "--push", purl]) pushUrl

main = do
    args <- getArgs
    home <- getEnv "HOME"
    let gitCacheDir = home </> ".gitcache"
    case args of
        "clone":"github":user:repo:[] -> do
            cloneUrl gitCacheDir
                     ("https://github.com/" ++ user ++ "/" ++ repo)
                     (Just ("git@github.com:" ++ user ++ "/" ++ repo))
        "clone":url:[] ->
            cloneUrl gitCacheDir url Nothing
        "list":[]    -> do
            repos <- filter (not . flip elem [".",".."]) <$> getDirectoryContents gitCacheDir
            mapM_ putStrLn repos
            -- FIXME open config file and parse origin url.
        _            -> do
            putStrLn "usage: gitcache <command>"
            mapM_ putStrLn
                [ "  clone <url>"
                , "  clone github <user> <repo>"
                , "  list"
                ]
