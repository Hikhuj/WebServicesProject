module RoomGrove exposing (main)

import Html exposing (h1, div, text, img)
import Html.Attributes exposing (..)

view model = 
 div [ class "content" ]
  [ h1 [] [ text "Room Grove" ]
  , div [ id "thumbnails" ]
   [ 
    img ["http://elm-in-action.com/1.jpeg"] [] 
    , img ["http://elm-in-action.com/2.jpeg"] [] 
    , img ["http://elm-in-action.com/3.jpeg"] []
   ]
  ]

main = 
 view "No model yet"
