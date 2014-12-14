#!/usr/bin/runhaskell
--
-- Simple git overlay that cache repository locally to the user,
-- for faster cloning and updating
--
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
urlToName s
    | isPrefixOf "http://" s  = httpUrl
    | isPrefixOf "https://" s = httpUrl
    | otherwise               = sshUrl
  where httpUrl = reverse $ fst $ break (== '/') $ reverse s
        sshUrl  = httpUrl

rawSystemEC s l = do
    ec <- rawSystem s l
    case ec of
        ExitSuccess   -> return ()
        ExitFailure i -> error ("call: " ++  intercalate " " (s : l) ++ " exit with " ++ show i)

withSetDirectory newDir f = do
    old <- getCurrentDirectory
    setCurrentDirectory newDir
    _ <- f
    setCurrentDirectory old

-- update everything in a cache repository
updateRepo repoDir =
    withSetDirectory repoDir $
        rawSystemEC "git" [ "fetch", "--all" ]

data CloneOpt = CloneLocally | CloneInCache
    deriving (Show,Eq)

cloneRepo inDir destName url =
    withSetDirectory inDir $
        rawSystemEC "git" [ "clone", "--mirror", url, destName ]

cloneUrl gitCacheDir cloneOpt url pushUrl = do
    let destName = urlToHash url
        destDir  = gitCacheDir </> destName
    clonedAlready <- doesDirectoryExist destDir
    if clonedAlready
        then updateRepo destDir
        else cloneRepo gitCacheDir destName url

    when (cloneOpt == CloneLocally) $ do
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
    getOriginUrl (_:xs) = getOriginUrl xs
    getOriginUrl []     = Nothing

    findUrl = find (isPrefixOf "url =" . stripSpaces)
    stripSpaces = dropWhile isSpace

showRepoUrl gitCacheDir repoDir = do
    url <- getRepoUrl gitCacheDir repoDir
    putStrLn (repoDir ++ ": " ++ maybe "error: cannot determine 'url'" id url)

listCacheRepos gitCacheDir =
    filter (not . flip elem [".",".."]) <$> getDirectoryContents gitCacheDir

initialization = do
    gitCacheDir <- getGitCacheDir
    mapM_ expectedDirectory [ gitCacheDir ]
    return gitCacheDir
  where
    expectedDirectory :: FilePath -> IO ()
    expectedDirectory = createDirectoryIfMissing False

    getGitCacheDir = flip (</>) ".gitcache" <$> getEnv "HOME"

commandClone gitCacheDir cloneOpt args =
    case args of
        "github":user:repo:[] -> do
            cloneUrl gitCacheDir cloneOpt
                     ("https://github.com/" ++ user ++ "/" ++ repo)
                     (Just ("git@github.com:" ++ user ++ "/" ++ repo))
        url:[] -> do
            cloneUrl gitCacheDir cloneOpt url Nothing
        _ ->
            error "clone options not known"

main = do
    args <- getArgs
    gitCacheDir <- initialization
    case args of
        "clone":cargs -> commandClone gitCacheDir CloneLocally cargs
        "cache":cargs -> commandClone gitCacheDir CloneInCache cargs
        "list":[]    -> do
            repos <- listCacheRepos gitCacheDir
            mapM_ (showRepoUrl gitCacheDir) repos
        "update":[]  -> do
            repos <- listCacheRepos gitCacheDir
            let nbRepos = length repos
            forM_ (zip [1..] repos) $ \(i :: Int,repo) -> do
                url <- getRepoUrl gitCacheDir repo
                putStrLn ("updating " ++ show i ++ "/" ++ show nbRepos ++ " " ++ maybe "" id url)
                updateRepo (gitCacheDir </> repo)
        _            -> do
            putStrLn "usage: gitcache <command>"
            mapM_ putStrLn
                [ "  clone <url>"
                , "  clone github <user> <repo>"
                , "  cache <url>"
                , "  cache github <user> <repo>"
                , "  list"
                , "  update"
                ]
