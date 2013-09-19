{-# LANGUAGE OverloadedStrings   #-}

-- | Contains web handlers to serve files from a directory.
module FileServe
(
  serveDirectory
) where

------------------------------------------------------------------------------

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans
import qualified Data.ByteString.Char8 as S
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.UTF8 as U
import           Data.List
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import           Data.Maybe (fromMaybe, isNothing)
import           System.Directory
import           System.FilePath
------------------------------------------------------------------------------
import           Snap.Core
import           Snap.Util.FileServe hiding (getSafePath, serveDirectoryWith, serveDirectory)


------------------------------------------------------------------------------
-- | Gets a path from the 'Request' using 'rqPathInfo' and makes sure it is
-- safe to use for opening files.  A path is safe if it is a relative path
-- and has no ".." elements to escape the intended directory structure.
getSafePath :: MonadSnap m => m FilePath
getSafePath = do
    req <- getRequest
    let mp = urlDecode $ rqPathInfo req

    p <- maybe pass (return . U.toString) mp

    -- relative paths only!
    when (not $ isRelative p) pass

    -- check that we don't have any sneaky .. paths
    let dirs = splitDirectories p
    when (elem ".." dirs) pass

    return $! joinPath dirs


------------------------------------------------------------------------------
lookupExt :: a -> HashMap FilePath a -> FilePath -> a
lookupExt def m f =
    if null ext
      then def
      else fromMaybe (lookupExt def m (drop 1 ext)) mbe

  where
    ext             = takeExtensions f
    mbe             = Map.lookup ext m

------------------------------------------------------------------------------
uriWithoutQueryString :: Request -> ByteString
uriWithoutQueryString rq = S.takeWhile (/= '?') uri
  where
    uri   = rqURI rq

------------------------------------------------------------------------------
queryStringSuffix :: Request -> ByteString
queryStringSuffix rq = S.concat [ s, qs ]
  where
    qs = rqQueryString rq
    s  = if S.null qs then "" else "?"

------------------------------------------------------------------------------
-- | Serves static files from a directory using the default configuration
-- as given in 'defaultDirectoryConfig'.
serveDirectory :: MonadSnap m
               => FilePath           -- ^ Directory to serve from
               -> m ()
serveDirectory = serveDirectoryWith defaultDirectoryConfig
{-# INLINE serveDirectory #-}


------------------------------------------------------------------------------
-- | Serves static files from a directory.  Configuration options are
-- passed in a 'DirectoryConfig' that captures various choices about desired
-- behavior.  The relative path given in 'rqPathInfo' is searched for a
-- requested file, and the file is served with the appropriate mime type if it
-- is found. Absolute paths and \"@..@\" are prohibited to prevent files from
-- being served from outside the sandbox.
serveDirectoryWith :: MonadSnap m
                   => DirectoryConfig m  -- ^ Configuration options
                   -> FilePath           -- ^ Directory to serve from
                   -> m ()
serveDirectoryWith cfg base = do
    b <- directory <|> file <|> redir
    when (not b) pass

  where

    idxs     = indexFiles cfg
    generate = indexGenerator cfg
    mimes    = mimeTypes cfg
    dyns     = dynamicHandlers cfg
    pshook   = preServeHook cfg

    -- Serves a file if it exists; passes if not
    serve f = do
        liftIO (doesFileExist f) >>= flip unless pass
        let fname          = takeFileName f
        let staticServe f' = pshook f >> serveFileAs (fileType mimes fname) f'
        lookupExt staticServe dyns fname f >> return True <|> return False

    -- Serves a directory via indices if available.  Returns True on success,
    -- False on failure to find an index.  Passes /only/ if the request was
    -- not for a directory (no trailing slash).
    directory = do
        rq  <- getRequest
        let uri = uriWithoutQueryString rq
        unless ("/" `S.isSuffixOf` uri) pass
        rel <- (base </>) <$> getSafePath
        b   <- liftIO $ doesDirectoryExist rel
        if b then do let serveRel f = serve (rel </> f)
                     foldl' (<|>) pass (Prelude.map serveRel idxs)
                         <|> (generate rel >> return True)
                         <|> return False
             else return False

    -- Serves a file requested by name.  Passes if the file doesn't exist.
    file = serve =<< ((base </>) <$> getSafePath)

    -- If the request is for a directory but lacks a trailing slash, redirects
    -- to the directory name with a trailing slash.
    redir = do
        rel <- (base </>) <$> getSafePath
        liftIO (doesDirectoryExist rel) >>= flip unless pass
        rq <- getRequest
        let uri = uriWithoutQueryString rq
        let qss = queryStringSuffix rq
        let u = S.concat [uri, "/", qss]
        redirect u

