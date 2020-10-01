{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes  #-}

import Text.Hamlet (HtmlUrl, hamlet)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Data.Text (Text)

data MyRoute = Home

render :: MyRoute -> [(Text, Text)] -> Text
render Home _ = "/Home"

footer :: HtmlUrl MyRoute
footer = [hamlet|
<footer>
 Return to #
 <a href=@{Home}>Homepage
|]

main :: IO ()
main = putStrLn $ renderHtml $ [hamlet|
<body>
 <p> Tis is my page.
 ^{footer}
|] render
