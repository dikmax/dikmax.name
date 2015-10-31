import Control.Monad
import Data.List
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import System.Directory (createDirectoryIfMissing)

buildDir = "_result"
tempDir = "_temp"
hakyllDir = "_site"
hakyllCacheDir = "_cache"
nodeModulesDir = "node_modules"
nodeModulesBinDir = nodeModulesDir </> ".bin"

highlightLanguages :: [String]
highlightLanguages = ["bash", "css", "haskell", "javascript", "markdown", "sql", "xml", "dart"]

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build", shakeThreads=0} $ do
    phony "clean" $ do
        putNormal $ "Cleaning files in _build"
        removeFilesAfter "_build" ["//*"]
        putNormal $ "Cleaning files in " ++ buildDir
        removeFilesAfter buildDir ["//*"]
        putNormal $ "Cleaning files in " ++ tempDir
        removeFilesAfter tempDir ["//*"]
        putNormal $ "Cleaning files in " ++ hakyllDir
        removeFilesAfter hakyllDir ["//*"]
        putNormal $ "Cleaning files in " ++ hakyllCacheDir
        removeFilesAfter hakyllCacheDir ["//*"]

    phony "init submodules" $ do
        () <- cmd "git" "submodule" "init"
        cmd "git" "submodule" "update"

    phony "build" $ do
        -- Test demos folder
        demosExists <- doesFileExist "demos/.git"
        when (not demosExists) $ need ["init submodules"]

        staticFiles <- getDirectoryFiles "." statics
        demosFiles <- getDirectoryFiles "." ["demos//*"]
        faviconsFiles <- getDirectoryFiles "." ["favicons//*"]
        need $ [buildDir </> x | x <- staticFiles]                                            -- Statics

            ++ [buildDir </> x | x <- demosFiles, not $ ".git" `isPrefixOf` dropDirectory1 x] -- Demos

            ++ [buildDir </> dropDirectory1 x | x <- faviconsFiles]                           -- Favicons

            ++ [ buildDir </> "css/style.css"                                                 -- Styles
               , buildDir </> "css/print.css"

               , buildDir </> "map/world.json"                                                -- Map

               , buildDir </> "dart/s.js"                                                     -- Scripts
               , buildDir </> "dart/smap.js"
               , buildDir </> "dart/script.dart.js"
               , buildDir </> "dart/script-route-planner.dart.js"
               , buildDir </> "dart/script-map.dart.js"

               , "site"
               ]


    -- haskell
    ["dikmax-name", "server"] &%> \_ -> do
        src <- getDirectoryFiles "." ["src//*.hs", "dikmax-name.cabal"]
        cmd "stack" "install"

    -- npm packages
    phony "npm install" $ do
        need ["package.json"]
        cmd "npm" "install"

    -- Statics
    forM_ statics buildStatic
    buildDemos
    buildFavicons

    buildStyles
    buildMap
    buildScripts

    buildSite

    where
        statics =
            [ "fonts/*"
            , "images//*.png", "images//*.jpg", "images//*.gif"
            , "robots.txt", "yandex-widget-manifest.json", "js/html5shiv.js", "map/data.json"
            ]

buildStatic pattern =
    buildDir </> pattern %> \out -> do
        let src = dropDirectory1 out
        copyFileChanged src out



buildDemos =
    buildDir </> "demos//*" %> \out -> do
        let src = dropDirectory1 out
        when (p src) $ copyFileChanged src out
    where
        p file = not $ ".git" `isPrefixOf` dropDirectory1 file



-- Copy favicons folder to root
buildFavicons =
    buildDir </> "*" %> \out -> do
        let src = "favicons" </> dropDirectory1 out
        exists <- doesFileExist src
        when exists $ copyFileChanged src out



-- Build styles
buildStyles = do
    let lessc = nodeModulesBinDir </> "lessc"
    lessc %> (\_ -> need ["npm install"])

    buildDir </> "css/*.css" %> \out -> do
        let src = "less" </> dropDirectory1 (dropDirectory1 out -<.> "less")
        files <- getDirectoryFiles "." ["less//*"]
        need (lessc : files)
        cmd lessc "--clean-css=advanced" "--include-path=less" src out


-- Build map
buildMap = do
    buildDir </> "map/world.json" %> \out -> do
        let countries = tempDir </> "countries.json"
        let subunits = tempDir </> "subunits.json"
        let regions = tempDir </> "regions.json"
        need [countries, subunits, regions]
        cmd "topojson" "-o" out "--id-property" "ADM_A3,SU_A3,adm1_code" "--simplify" "1e-6" "--" countries subunits regions

    tempDir </> "subunits.json" %> \out -> do
        liftIO $ removeFiles "." [out]
        let src = "map/ne_10m_admin_0_map_subunits/ne_10m_admin_0_map_subunits.shp"
        need [src]
        cmd Shell "ogr2ogr" "-f" "GeoJSON" out "-where" "\"ADM0_A3 = 'FRA'\"" src

    tempDir </> "countries.json" %> \out -> do
        liftIO $ removeFiles "." [out]
        let src = "map/ne_10m_admin_0_map_subunits/ne_10m_admin_0_map_subunits.shp"
        need [src]
        cmd Shell "ogr2ogr" "-f" "GeoJSON" out "-where" "\"ADM0_A3 != 'FRA' and ADM0_A3 != 'RUS' and ADM0_A3 != 'USA'\"" src

    tempDir </> "regions.json" %> \out -> do
        liftIO $ removeFiles "." [out]
        let src = "map/ne_10m_admin_1_states_provinces_lakes/ne_10m_admin_1_states_provinces_lakes.shp"
        need [src]
        cmd Shell "ogr2ogr" "-f" "GeoJSON" out "-where" "\"ADM0_A3 = 'RUS' or ADM0_A3 = 'USA'\"" src



-- Build scripts
buildScripts = do
    buildDir </> "dart/s.js" %> \out -> do
        content <- concatResources ["js/highlight.js/build/highlight.pack.js", "js/likely/likely.js"]
        writeFile' out content

    buildDir </> "dart/smap.js" %> \out -> do
        content <- concatResources ["js/d3/d3.min.js", "js/polyhedron.js", "js/topojson/topojson.min.js"]
        writeFile' out content

    phony "npm install highlight.js" $ do
        need ["js/highlight.js/package.json"]
        cmd (Cwd "js/highlight.js") "npm" "install"

    "js/highlight.js/build/highlight.pack.js" %> \out -> do
        files <- getDirectoryFiles "." ["js/highlight.js/src//*.js", "js/highlight.js/tools//*.js"]
        need ("npm install highlight.js" : files)
        cmd (Cwd "js/highlight.js") "node" "tools/build.js" highlightLanguages

    "dart/pubspec.lock" %> \_ -> do
        need ["dart/pubspec.yaml"]
        () <- cmd (Cwd "dart") "pub" "get"
        files <- getDirectoryFiles "." ["dart/packages//*"]
        need files

    buildDart "dart/script.dart.js" "dart/web/main.dart"
    buildDart "dart/script-route-planner.dart.js" "dart/web/route-planner.dart"
    buildDart "dart/script-map.dart.js" "dart/web/map.dart"

    where
        concatResources resources = do
            need resources
            content <- mapM readFile' resources
            return $ concat content

        buildDart res src =
            buildDir </> res %> \out -> do
                need [src, "dart/pubspec.lock"]
                () <- cmd "dart2js" ("--out=" ++ out) "--minify" src
                -- deps <- readFileLines $ out -<.> "js.deps"
                -- need $ map (drop 7) deps
                removeFilesAfter "." [out -<.> "js.deps", out -<.> "js.map"]

buildSite =
    phony "site" $ do
        files <- getDirectoryFiles ""
            [ "comments/*.html"
            , "templates/*.html"
            , "route-planner/index.html"
            , "collections/*.txt"
            , "index.md"
            , "404.md"
            , "about.md"
            , "projects.md"
            , "post//*"
            ]
        need ("dikmax-name" : files)
        () <- cmd "./dikmax-name build"
        results <- getDirectoryFiles "" [hakyllDir <//> "*"]
        forM_ results (\file -> do
            let out = buildDir </> dropDirectory1 file
            liftIO $ createDirectoryIfMissing True (takeDirectory out)
            copyFileChanged file out)
        putNormal $ "Hakyll build completed"
