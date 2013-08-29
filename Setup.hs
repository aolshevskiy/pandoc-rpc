import Distribution.Simple
import Distribution.PackageDescription
import Distribution.Simple.Setup
import System.Process

main = defaultMainWithHooks simpleUserHooks { preBuild = preBuildHook }

preBuildHook :: Args -> BuildFlags -> IO HookedBuildInfo 
preBuildHook _ _ = do
  h <- runProcess "hprotoc" ["-d", "dist/build/autogen", "pandoc.proto"] Nothing Nothing Nothing Nothing Nothing
  waitForProcess h
  return emptyHookedBuildInfo