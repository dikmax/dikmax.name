import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build"} $ do
    want ["build"]

    phony "clean" $ do
        putNormal "Cleaning files in _build"
        removeFilesAfter "_build" ["//*"]

    phony "build" $ do
        need ["statics"]
        -- cs <- getDirectoryFiles "" ["_build/fonts/*"]
        -- need cs

    phony "statics" $ do
        cs <- getDirectoryFiles "" ["fonts/*"]
        need cs
        mapM_ (\f -> copyFileChanged f ("_build" </> f)) cs

