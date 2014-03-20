{-# LANGUAGE OverloadedStrings   #-}

-- | Contains web handlers to serve files from a directory.
module FileServe
(
  serveDirectory,
  serveFile
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
import           Data.Maybe (fromMaybe)
import           System.Directory
import           System.FilePath
------------------------------------------------------------------------------
import           Snap.Core
import           Snap.Util.FileServe hiding (getSafePath, serveDirectoryWith, serveDirectory,
                   defaultDirectoryConfig, defaultMimeTypes)


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

defaultMimeTypes :: MimeMap
defaultMimeTypes = Map.fromList [
  ( ".asc"     , "text/plain"                        ),
  ( ".asf"     , "video/x-ms-asf"                    ),
  ( ".asx"     , "video/x-ms-asf"                    ),
  ( ".avi"     , "video/x-msvideo"                   ),
  ( ".bz2"     , "application/x-bzip"                ),
  ( ".c"       , "text/plain"                        ),
  ( ".class"   , "application/octet-stream"          ),
  ( ".conf"    , "text/plain"                        ),
  ( ".cpp"     , "text/plain"                        ),
  ( ".css"     , "text/css"                          ),
  ( ".cxx"     , "text/plain"                        ),
  ( ".dart"    , "application/dart"                  ),
  ( ".dtd"     , "text/xml"                          ),
  ( ".dvi"     , "application/x-dvi"                 ),
  ( ".gif"     , "image/gif"                         ),
  ( ".gz"      , "application/x-gzip"                ),
  ( ".hs"      , "text/plain"                        ),
  ( ".htm"     , "text/html; charset=utf-8"          ),
  ( ".html"    , "text/html; charset=utf-8"          ),
  ( ".ico"     , "image/x-icon"                      ),
  ( ".jar"     , "application/x-java-archive"        ),
  ( ".jpeg"    , "image/jpeg"                        ),
  ( ".jpg"     , "image/jpeg"                        ),
  ( ".js"      , "text/javascript"                   ),
  ( ".json"    , "application/json"                  ),
  ( ".log"     , "text/plain"                        ),
  ( ".m3u"     , "audio/x-mpegurl"                   ),
  ( ".mov"     , "video/quicktime"                   ),
  ( ".mp3"     , "audio/mpeg"                        ),
  ( ".mpeg"    , "video/mpeg"                        ),
  ( ".mpg"     , "video/mpeg"                        ),
  ( ".ogg"     , "application/ogg"                   ),
  ( ".pac"     , "application/x-ns-proxy-autoconfig" ),
  ( ".pdf"     , "application/pdf"                   ),
  ( ".png"     , "image/png"                         ),
  ( ".ps"      , "application/postscript"            ),
  ( ".qt"      , "video/quicktime"                   ),
  ( ".sig"     , "application/pgp-signature"         ),
  ( ".spl"     , "application/futuresplash"          ),
  ( ".svg"     , "image/svg+xml"                     ),
  ( ".swf"     , "application/x-shockwave-flash"     ),
  ( ".tar"     , "application/x-tar"                 ),
  ( ".tar.bz2" , "application/x-bzip-compressed-tar" ),
  ( ".tar.gz"  , "application/x-tgz"                 ),
  ( ".tbz"     , "application/x-bzip-compressed-tar" ),
  ( ".text"    , "text/plain"                        ),
  ( ".tgz"     , "application/x-tgz"                 ),
  ( ".torrent" , "application/x-bittorrent"          ),
  ( ".ttf"     , "application/x-font-truetype"       ),
  ( ".txt"     , "text/plain"                        ),
  ( ".rss"     , "application/rss+xml"               ),
  ( ".wav"     , "audio/x-wav"                       ),
  ( ".wax"     , "audio/x-ms-wax"                    ),
  ( ".wma"     , "audio/x-ms-wma"                    ),
  ( ".wmv"     , "video/x-ms-wmv"                    ),
  ( ".xbm"     , "image/x-xbitmap"                   ),
  ( ".xml"     , "text/xml"                          ),
  ( ".xpm"     , "image/x-xpixmap"                   ),
  ( ".xwd"     , "image/x-xwindowdump"               ),
  ( ".zip"     , "application/zip"                   ) ]


------------------------------------------------------------------------------
-- | A reasonable default configuration for directory serving.  This
-- configuration uses built-in MIME types from 'defaultMimeTypes', serves
-- common index files @index.html@ and @index.htm@, but does not autogenerate
-- directory indexes, nor have any dynamic file handlers. The 'preServeHook'
-- will not do anything.
defaultDirectoryConfig :: MonadSnap m => DirectoryConfig m
defaultDirectoryConfig = DirectoryConfig {
    indexFiles = ["index.html", "index.htm"],
    indexGenerator = const pass,
    dynamicHandlers = Map.fromList[
      ( ".html", serveHtml)
    ],
    mimeTypes = defaultMimeTypes,
    preServeHook = const $ return ()
    }
    where
        serveHtml file = do
            modifyResponse $ setHeader "Cache-Control" "no-cache, no-store, must-revalidate"
                           . setHeader "Pragma" "no-cache"
                           . setHeader "Expires" "0"
            serveFileAs "text/html; charset=utf-8" file

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

