module Room exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (Html, button, div, h1, h2, img, text, label, input)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random

urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


isEmpty : String -> Bool
isEmpty str =
    str == ""


selectedRoom : { description : String, data : String }
selectedRoom =
    { description = "ClickedRoom", data = "1.jpeg" }


type Msg 
        = ClickedRoom String
        | GotSelectedIndex Int
        | ClickedSize ThumbnailSize
        | ClickedSurpriseMe



view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ h1 [] [ text "Virtual Hotel" ]
        , h2 [] [ text "Rooms" ]
        , button [ onClick ClickedSurpriseMe ] [ text "Sorprendeme!" ]
        , div [ id "thumbnails", class ( sizeToString model.chosenSize ) ] (List.map (viewThumbnail model.selectedUrl) model.rooms)
        , div [ id "choose-size" ] (List.map viewSizeChooser [ Small, Medium, Large ])
        , img [ class "large", src (urlPrefix ++ "large/" ++ model.selectedUrl) ] []
        ]


viewThumbnail : String -> Room -> Html Msg
viewThumbnail selectedUrl thumb =
    img [ src (urlPrefix ++ thumb.url), classList [ ( "selected", selectedUrl == thumb.url ) ], onClick (ClickedRoom thumb.url) ] []

viewSizeChooser : ThumbnailSize -> Html Msg
viewSizeChooser size = 
        label []
        [ input [ type_ "radio", name "size", onClick (ClickedSize size) ] []
        , text (sizeToString size) 
        ]

sizeToString : ThumbnailSize -> String
sizeToString size =
        case size of 
                Small -> "small"
                Medium -> "medium"
                Large -> "large"

type ThumbnailSize 
        = Small
        | Medium
        | Large


type alias Room =
    { url : String }


type Status = 
        Loaded ( List Room ) String
        | Loading
        | Error
                
type alias Model =
    { rooms : List Room
    , selectedUrl : String
    , chosenSize : ThumbnailSize
    }


initialModel : Model
initialModel =
    { rooms =
        [ { url = "1.jpeg" }
        , { url = "2.jpeg" }
        , { url = "3.jpeg" }
        ]
    , selectedUrl = "1.jpeg"
    , chosenSize = Large
    }


roomArray : Array Room
roomArray =
    Array.fromList initialModel.rooms


getRoomUrl : Int -> String 
getRoomUrl index = 
        case Array.get index roomArray of 
                Just room ->
                        room.url
                Nothing -> 
                        ""

randomRoomPicker : Random.Generator Int
randomRoomPicker =
        Random.int 0 ( Array.length roomArray - 1 )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSelectedIndex index -> 
            ( { model | selectedUrl = getRoomUrl index }, Cmd.none )
        ClickedRoom url ->
            ( { model | selectedUrl = url }, Cmd.none)

        ClickedSize size -> 
            ( { model | chosenSize = size }, Cmd.none)
        ClickedSurpriseMe ->
            ( model, Random.generate GotSelectedIndex randomRoomPicker )


main : Program () Model Msg
main =
    Browser.element
        { init = \flags -> ( initialModel , Cmd.none )
        , view = view
        , update = update
        , subscriptions = \model -> Sub.none
        }
