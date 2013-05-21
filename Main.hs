{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad
import Data.Monoid
import System.Directory
import System.Environment (getArgs, withArgs)
import System.FilePath

import qualified System.Posix as Posix
import System.IO.Error
import Control.Exception
import qualified Data.ByteString.Char8 as BS
import System.Posix.ByteString.FilePath
import System.Posix.Directory.ByteString as PosixBS
import System.Posix.Files.ByteString
import System.Process (system)
import Criterion.Main


import System.IO.Unsafe


listFilesRecursive :: FilePath -> IO [FilePath]
listFilesRecursive topdir = do
    -- names <- getDirectoryContents topdir
    names <- getDirectoryContents topdir
    let properNames = filter (`notElem` [".", ".."]) names
    paths <- forM properNames $ \name -> do
        let path = topdir </> name
        isDirectory <- doesDirectoryExist path
        if isDirectory
            then listFilesRecursive path
            else return [path]
    return (topdir : concat paths)



listFilesRecursive2 :: FilePath -> IO ()
listFilesRecursive2 path = do
    putStrLn path
    -- BS.putStrLn (BS.pack path) -- no improvement

    isDirectory <- doesDirectoryExist path

    when isDirectory $ find path listFilesRecursive2



find :: FilePath -> (FilePath -> IO ()) -> IO ()
find path f =
  modifyIOError ((`ioeSetFileName` path) .
                 (`ioeSetLocation` "find")) $ do
    bracket
      (Posix.openDirStream path)
      Posix.closeDirStream
      loop
 where
  loop dirp = do
    e <- Posix.readDirStream dirp
    unless (e == "") $ do
      unless (e == "." || e == "..") $ f (path </> e)
      loop dirp




listFilesRecursive3 :: RawFilePath -> IO ()
listFilesRecursive3 path = do
    BS.putStrLn path

    isDir <- isDirectory <$> getFileStatus path

    when isDir $ findBS path listFilesRecursive3



findBS :: RawFilePath -> (RawFilePath -> IO ()) -> IO ()
findBS path f =
  modifyIOError ((`ioeSetFileName` (BS.unpack path)) .
                 (`ioeSetLocation` "findBS")) $ do
    bracket
      (PosixBS.openDirStream path)
      PosixBS.closeDirStream
      loop
 where
  loop dirp = do
    e <- PosixBS.readDirStream dirp
    unless (e == "") $ do
      unless (e == "." || e == "..") $ f (path <> "/" <> e)
      loop dirp




listFilesRecursiveBS :: RawFilePath -> IO [RawFilePath]
listFilesRecursiveBS topdir = do
    names <- getDirectoryContentsBS topdir
    let properNames = filter (`notElem` [".", ".."]) names
    paths <- forM properNames $ \name -> do
        let path = topdir <> "/" <> name -- TODO PROBLEM
        isDir <- isDirectory <$> getFileStatus path
        if isDir
            then listFilesRecursiveBS path
            else return [path]
    return (topdir : concat paths)



getDirectoryContentsBS :: RawFilePath -> IO [RawFilePath]
getDirectoryContentsBS path =
  modifyIOError ((`ioeSetFileName` (BS.unpack path)) .
                 (`ioeSetLocation` "getDirectoryContentsBS")) $ do
    bracket
      (PosixBS.openDirStream path)
      PosixBS.closeDirStream
      loop
 where
  loop dirp = do
     e <- PosixBS.readDirStream dirp
     if BS.null e then return [] else do
       es <- loop dirp
       return (e:es)




listFilesRecursiveBSLazy :: RawFilePath -> IO [RawFilePath]
listFilesRecursiveBSLazy topdir = do
    names <- getDirectoryContentsBS topdir
    let properNames = filter (`notElem` [".", ".."]) names
    paths <- forM properNames $ \name -> unsafeInterleaveIO $ do
        let path = topdir <> "/" <> name -- TODO PROBLEM
        isDir <- isDirectory <$> getFileStatus path
        if isDir
            then listFilesRecursiveBSLazy path
            else return [path]
    return (topdir : concat paths)




-- getDirectoryContentsBS2 :: RawFilePath -> IO [RawFilePath]
-- getDirectoryContentsBS2 path =
--   modifyIOError ((`ioeSetFileName` (BS.unpack path)) .
--                  (`ioeSetLocation` "getDirectoryContentsBS")) $ do
--     bracket
--       (PosixBS.openDirStream path)
--       PosixBS.closeDirStream
--       (loop id)
--  where
--   loop cont dirp = do
--      e <- PosixBS.readDirStream dirp
--      if BS.null e then return [] else do
--        es <- loop dirp
--        return (e:es)


-- TODO catch "no such file" errors to make the benchmark run more easily
-- TODO create directory structure ourselves

main :: IO ()
main = do
  d:otherArgs <- getArgs

  putStrLn $ unlines
      [ "BENCHMARK NOTE: Depending on the directory given as 1st arguement, this might take long."
      , "Use --samples=N to reduce the time to an acceptable amount."
      ]

  withArgs otherArgs $ defaultMain

    [ bench "getDirectoryContents"     $ nfIO $ listFilesRecursive d
    , bench "find style"               $ nfIO $ listFilesRecursive2 d
    , bench "findBS"                   $ nfIO $ listFilesRecursive3 (BS.pack d)
    , bench "listFilesRecursiveBS"     $ nfIO $ listFilesRecursiveBS (BS.pack d)
    , bench "listFilesRecursiveBSLazy" $ nfIO $ listFilesRecursiveBSLazy (BS.pack d)
    , bench "unix find"                $ nfIO $ void $ system ("find " ++ d)
    ]
