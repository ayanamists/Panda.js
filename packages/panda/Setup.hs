import Distribution.Simple
import Distribution.AppImage

image :: AppImage
image = AppImage
  { appName = "panda-exe"
  , appIcons = []
  , appDesktop = ""
  , appResources = []
  , appDirCustom = Nothing
  }

main = defaultMainWithHooks defaultUserHooks
         { postBuild = appImagePostBuildHook
         }
