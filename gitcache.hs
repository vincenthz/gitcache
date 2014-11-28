#!/usr/bin/runhaskell
--
-- Simple git overlay that cache repository locally to the user,
-- for faster cloning and updating
--
{-# LANGUAGE PackageImports #-}
module Main where

import Data.List
import Data.Char
import Control.Applicative
import Control.Monad
import System.Environment
import System.Directory
import System.FilePath
import System.Process
import System.Exit
import "cryptohash" Crypto.Hash
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

-- update everything in a cache repository
updateRepo repoDir =
    withSetDirectory repoDir $
        rawSystemEC "git" [ "fetch", "--all" ]

cloneRepo inDir destName url =
    withSetDirectory inDir $
        rawSystemEC "git" [ "clone", "--mirror", url, destName ]

cloneUrl gitCacheDir url pushUrl = do
    let destName = urlToHash url
        destDir  = gitCacheDir </> destName
    clonedAlready <- doesDirectoryExist destDir
    if clonedAlready
        then updateRepo destDir
        else cloneRepo gitCacheDir destName url
    -- and clone locally and replace the origin url
    rawSystemEC "git" [ "clone", destDir, urlToName url ]
    withSetDirectory (urlToName url) $ do
        rawSystemEC "git" [ "remote", "set-url", "origin", url ]
        maybe (return ()) (\purl -> rawSystemEC "git" [ "remote", "set-url", "origin", "--push", purl]) pushUrl

getRepoUrl gitCacheDir repoDir =
    getOriginUrl . lines <$> readFile (gitCacheDir </> repoDir </> "config")
  where
    getOriginUrl ("[remote \"origin\"]":l) =
        stripSpaces . drop 5 . stripSpaces <$> findUrl l
    getOriginUrl (x:xs) = getOriginUrl xs

    findUrl = find (isPrefixOf "url =" . stripSpaces)
    stripSpaces = dropWhile isSpace

showRepoUrl gitCacheDir repoDir = do
    url <- getRepoUrl gitCacheDir repoDir
    putStrLn (repoDir ++ ": " ++ maybe "error: cannot determine 'url'" id url)

listCacheRepos gitCacheDir =
    filter (not . flip elem [".",".."]) <$> getDirectoryContents gitCacheDir

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
            repos <- listCacheRepos gitCacheDir
            mapM_ (showRepoUrl gitCacheDir) repos
        "update":[]  -> do
            repos <- listCacheRepos gitCacheDir
            let nbRepos = length repos
            forM_ (zip [1..] repos) $ \(i,repo) -> do
                url <- getRepoUrl gitCacheDir repo
                putStrLn ("updating " ++ show i ++ "/" ++ show nbRepos ++ " " ++ maybe "" id url)
                updateRepo (gitCacheDir </> repo)
        _            -> do
            putStrLn "usage: gitcache <command>"
            mapM_ putStrLn
                [ "  clone <url>"
                , "  clone github <user> <repo>"
                , "  list"
                , "  update"
                ]
