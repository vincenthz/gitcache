#!/usr/bin/runhaskell
--
-- Simple git overlay that cache repository locally to the user,
-- for faster cloning and updating
--
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Application.CLI
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

commandGithubClone :: GitCacheConfig -> String -> String -> CloneOpt -> IO ()
commandGithubClone cfg user repo cloneOpt =
    cloneUrl (cacheDir cfg) cloneOpt
             ("https://github.com/" ++ user ++ "/" ++ repo)
             (Just ("git@github.com:" ++ user ++ "/" ++ repo))

commandURLClone :: GitCacheConfig -> String -> CloneOpt -> IO ()
commandURLClone cfg url cloneOpt =
    cloneUrl (cacheDir cfg) cloneOpt url Nothing

data CloneCLI = CloneCLI GitCacheConfig
instance CLI CloneCLI where
    name    _ = "clone"
    desc    _ = "Cache and clone the given repository"
    options _ = [ OptHelp []         (Just "url")  "The remote repository url (if non-github)"
                , OptHelp ["github"] (Just "username") "The remote repository is hosted on github.com into <user> account repository <repo>"
                , OptHelp []         (Just "repo") "the repository name"
                ]
    action (CloneCLI cfg) _ =
        withOptionalParameterStr ["github"] $ \mUserName ->
            case mUserName of
                Nothing       -> withStr "url"  $ \url  -> execute $ commandURLClone cfg url CloneLocally
                Just username -> withStr "repo" $ \repo -> execute $ commandGithubClone cfg username repo CloneLocally

data CacheCLI = CacheCLI GitCacheConfig
instance CLI CacheCLI where
    name    _ = "cache"
    desc    _ = "Cache the given repository"
    options _ = [ OptHelp []         (Just "url")  "The remote repository url (if non-github)"
                , OptHelp ["github"] (Just "username") "The remote repository is hosted on github.com into <user> account repository <repo>"
                , OptHelp []         (Just "repo") "the repository name"
                ]
    action (CacheCLI cfg) _ = do
        withOptionalParameterStr ["github"] $ \mUserName ->
            case mUserName of
                Nothing       -> withStr "url"  $ \url  -> execute $ commandURLClone cfg url CloneInCache
                Just username -> withStr "repo" $ \repo -> execute $ commandGithubClone cfg username repo CloneInCache

data ListCLI = ListCLI GitCacheConfig
instance CLI ListCLI where
    name    _ = "list"
    desc    _ = "List the cached repositories"
    options _ = []
    action (ListCLI cfg) _ =
        execute $ do
            let gitCacheDir = cacheDir cfg
            repos <- listCacheRepos gitCacheDir
            mapM_ (showRepoUrl gitCacheDir) repos

data UpdateCLI = UpdateCLI GitCacheConfig
instance CLI UpdateCLI where
    name    _ = "update"
    desc    _ = "Update the cached repositories"
    options _ = []
    action (UpdateCLI cfg) _ =
        execute $ do
            let gitCacheDir = cacheDir cfg
            repos <- listCacheRepos gitCacheDir
            let nbRepos = length repos
            forM_ (zip [1..] repos) $ \(i :: Int,repo) -> do
                url <- getRepoUrl gitCacheDir repo
                putStrLn ("updating " ++ show i ++ "/" ++ show nbRepos ++ " " ++ maybe "" id url)
                updateRepo (gitCacheDir </> repo)

data GitCacheConfig = GitCacheConfig
    { cacheDir :: FilePath
    } deriving (Show, Eq)

initialization :: IO GitCacheConfig
initialization = do
    gitCacheDir <- getGitCacheDir
    mapM_ expectedDirectory [ gitCacheDir ]
    return $ GitCacheConfig gitCacheDir
  where
    expectedDirectory :: FilePath -> IO ()
    expectedDirectory = createDirectoryIfMissing False

    getGitCacheDir = flip (</>) ".gitcache" <$> getEnv "HOME"

main :: IO ()
main = do
    cfg <- initialization
    defaultMain
        $ with Help
        $ with (CloneCLI cfg)
        $ with (CacheCLI cfg)
        $ with (ListCLI cfg)
        $ with (UpdateCLI cfg)
        $ initialize "Command Line Tool for Git Repositories Local Caching"
