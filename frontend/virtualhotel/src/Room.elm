{- Room View -}


module Room exposing (main)

-- Browser elements and sandbox
-- Html
-- Html Attributes
-- Html Events
-- Random library
-- JSON Decoders (Official and Third Party, for large decodings)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, classList, id, name, src, title, type_)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder, bool, int, list, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)
import Random



-- Urlprefix (WIP. should be moved to .env)


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"



-- Selected Room record constant


selectedRoom : { description : String, data : String }
selectedRoom =
    { description = "ClickedRoom", data = "1.jpeg" }



-- Msg type. What we expect to get from our REST API


type Msg
    = ClickedRoom String
    | GotRandomRoom Room
    | ClickedSize ThumbnailSize
    | ClickedSurpriseMe
    | GotRooms (Result Http.Error (List Room))


view : Model -> Html Msg
view model =
    div [ class "content" ] <|
        case model.status of
            Loaded rooms selectedUrl ->
                viewLoaded rooms selectedUrl model.chosenSize

            Loading ->
                []

            Errored errorMessage ->
                [ text ("Error: " ++ errorMessage) ]


viewLoaded : List Room -> String -> ThumbnailSize -> List (Html Msg)
viewLoaded rooms selectedUrl chosenSize =
    [ h1 [] [ text "Room Grove" ]
    , h2 [] [ text "Virtual Hotel" ]
    , button [ onClick ClickedSurpriseMe ] [ text "Sorprendeme!" ]
    , table [ class "table table--responsive" ]
        [ thead []
            [ tr []
                [ th [] [ text "Id" ], th [] [ text "Name" ], th [] [ text "Price" ] , th [] [ text "Status"]  ]
            ]
        , tbody []
            [ tr []
                [ td [] [ text " 1 " ]
                , td [] [ text "Iscus" ]
                , td [] [ text "$30.99" ]
                , td [] [ button [ class "button button--small button--green" ] [ text "Book!" ] ]
                ]
            ]
        ]

    {--, h3 [] [ text "Thumbnail Size: " ]
      - , div [ id "choose-size" ] (List.map viewSizeChooser [ Small, Medium, Large ]) --}
    , div [ id "thumbnails", class (sizeToString chosenSize) ] (List.map (viewThumbnail selectedUrl) rooms)
    , img [ class "large", src (urlPrefix ++ "large/" ++ selectedUrl) ] []
    ]


viewThumbnail : String -> Room -> Html Msg
viewThumbnail selectedUrl thumb =
    img
        [ src (urlPrefix ++ thumb.url)
        , title (thumb.title ++ " [" ++ String.fromInt thumb.size ++ "KB]")
        , classList [ ( "selected", selectedUrl == thumb.url ) ]
        , onClick (ClickedRoom thumb.url)
        ]
        []


viewSizeChooser : ThumbnailSize -> Html Msg
viewSizeChooser size =
    label []
        [ input [ type_ "radio", name "size", onClick (ClickedSize size) ] []
        , text (sizeToString size)
        ]


sizeToString : ThumbnailSize -> String
sizeToString size =
    case size of
        Small ->
            "small"

        Medium ->
            "medium"

        Large ->
            "large"


type ThumbnailSize
    = Small
    | Medium
    | Large


type alias Room =
    { id: Int
 , room_number : Int
 , capacity : Int
 , type_ : String
 , description : String
 , state :  String
 , priceNumerator : Int
 , priceDenominator : Int
    }



-- Room decoder fromJSON


roomDecoder : Decoder Room
roomDecoder =
    succeed Room
        |> required "url" string
        |> required "size" int
        |> optional "title" string "(untitled)"


type Status
    = Loaded (List Room) String
    | Loading
    | Errored String


type alias Model =
    { status : Status
    , chosenSize : ThumbnailSize
    }


initialModel : Model
initialModel =
    { status = Loading
    , chosenSize = Medium
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotRandomRoom room ->
            ( { model | status = selectUrl room.url model.status }, Cmd.none )

        ClickedRoom url ->
            ( { model | status = selectUrl url model.status }, Cmd.none )

        ClickedSize size ->
            ( { model
                | chosenSize = size
              }
            , Cmd.none
            )

        ClickedSurpriseMe ->
            case model.status of
                Loaded (firstRoom :: otherRooms) _ ->
                    Random.uniform firstRoom otherRooms
                        |> Random.generate GotRandomRoom
                        |> Tuple.pair model

                Loaded [] _ ->
                    ( model, Cmd.none )

                Loading ->
                    ( model, Cmd.none )

                Errored errorMessage ->
                    ( model, Cmd.none )

        GotRooms (Ok rooms) ->
            case rooms of
                first :: rest ->
                    ( { model | status = Loaded rooms first.url }, Cmd.none )

                [] ->
                    ( { model | status = Errored "Ninguna Habitacion Encontrada" }, Cmd.none )

        GotRooms (Err _) ->
            ( model, Cmd.none )


selectUrl : String -> Status -> Status
selectUrl url status =
    case status of
        Loaded rooms _ ->
            Loaded rooms url

        Loading ->
            status

        Errored errorMessage ->
            status



{--Initial Command for retrieving information from server, as an HTTP GET request  --}


initialCmd : Cmd Msg
initialCmd =
    Http.get
        { url = "http://elm-in-action.com/photos/list.json"
        , expect = Http.expectJson GotRooms (list roomDecoder)
        }


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, initialCmd )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
