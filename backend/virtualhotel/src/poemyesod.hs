{-#LANGUAGE OverloadedStrings  #-}
{-#LANGUAGE QuasiQuotes #-}
{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE TypeFamilies #-}

import Yesod

data PoemResources = PoemResources

mkYesod "PoemResources" [parseRoutes|
/ HomeR GET
|]
instance Yesod PoemResources

getHomeR = defaultLayout [whamlet|<p>Hello this is</p><br/><p>My precious world</p><br/><a href=https://www.google.com> Lets head outside!|]

main = warp 3000 PoemResources
