{- Rooms View -}


module Rooms exposing (Model, Msg, init, update, view)

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
import Json.Decode exposing (Decoder, bool, float, int, list, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)
import Constants as C
import Model exposing (Room)


-- Selected Room record constant
-- Msg type. What we expect to get from our REST API


type Msg
    = GotRooms (Result Http.Error (List Room))
    | DeleteRoom Int


view : Model -> Html Msg
view model =
    div [ class "content" ] <|
        case model.status of
            Loaded rooms ->
                viewLoaded rooms

            Loading ->
                [ div [ class "loadingSpinner" ]
                    [ span [ class "loadingSpinner-inner" ] []
                    , span [ class "loadingSpinner-inner" ] []
                    , span [ class "loadingSpinner-inner" ] []
                    , span [ class "loadingSpinner-inner" ] []
                    ]
                ]

            Errored errorMessage ->
                [ text ("Error: " ++ errorMessage) ]


viewLoaded : List Room -> List (Html Msg)
viewLoaded rooms =
    [ h3 [ class "text-huge text-black text-withSubtitle" ] [ text "Virtual Hotel" ]
    , h4 [ class "text-big text-gray m-none" ] [ text "Rooms" ] 
    , table [ class "table table--responsive" ]
        [ thead []
            [ tr []
                [ th [] [ text "Id" ], th [] [ text "Name" ], th [] [ text "Price" ], th [] [ text "Hotel" ], th [] [ text "Room Capacity" ], th [] [ text "Status" ] ]
            ]
        , tbody [] (List.map viewRoom  rooms )
        ]

    {--, h3 [] [ text "Room Size: " ]
      - , div [ id "choose-size" ] (List.map viewSizeChooser [ Small, Medium, Large ]) --}
    {--, div [ id "rooms", class (sizeToString chosenSize) ] (List.map (viewRoom selectedUrl) rooms) --}
    {--, img [ class "large", src (urlPrefix ++ "large/" ++ selectedUrl) ] [] --}
    ]


viewRoom : Room -> Html Msg
viewRoom room =
    tr []
        [ td [] [ text (String.fromInt room.id) ]
        , td [] [ text room.hab_descripcion ]
        , td [] [ text (String.fromFloat room.hab_precio) ]
        , td [] [ text (String.fromInt room.fk_hot_codigo) ]
        , td [] [ text (String.fromInt room.hab_capacidad) ]
        , td [] [ button [ class "button button--small button--green"] [ text "View!" ], button [ onClick (DeleteRoom room.id), class "button button--small button--primary"] [ text "Remove!" ] ]
        ]




-- Room decoder fromJSON


roomDecoder : Decoder Room
roomDecoder =
    succeed Room
        |> required "hab_capacidad" int
        |> required "hab_descripcion" string
        |> required "hab_estado" string
        |> required "fk_hot_codigo" int
        |> required "hab_numero" int
        |> required "id" int
        |> required "hab_tipo" string
        |> required "hab_precio" float


type Status
    = Loaded (List Room)
    | Loading
    | Errored String


type alias Model =
    { status : Status
    , selectedRoom : Int
    }


initialModel : Model
initialModel =
    { status = Loading
    , selectedRoom = 0
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
        case msg of 
        DeleteRoom roomId ->
            ( { model |  selectedRoom = roomId }, deleteCmd roomId  )
        GotRooms (Ok rooms) ->
            case rooms of
                first :: rest ->
                    ( { model | status = Loaded rooms }, Cmd.none )

                [] ->
                    ( { model | status = Errored "Ninguna Habitacion Encontrada" }, Cmd.none )

        GotRooms (Err _) ->
            ( { model | status = Errored "An Unknown Error Ocurred. Please Try Again Later" }, Cmd.none )


{--Initial Command for retrieving information from server, as an HTTP GET request  --}

roomsApiUrl : String
roomsApiUrl = 
        C.apiUrl ++ "rooms"

initialCmd : Cmd Msg
initialCmd =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" ("Bearer " ++ "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJqd3QiOjZ9.eVgBafOwYssLx9tn_skX3CdE7PAVNyp0oisYibH7Xss") ]
        , url = roomsApiUrl
        , body = Http.emptyBody
        , expect = Http.expectJson GotRooms (list roomDecoder)
        , timeout = Nothing
        , tracker = Nothing
        }

{-- PATCH / DELETE COMMAND  --}

deleteCmd : Int -> Cmd Msg
deleteCmd roomId =
    Http.request
        { method = "PATCH"
        , headers = [ Http.header "Authorization" ("Bearer " ++ "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJqd3QiOjZ9.eVgBafOwYssLx9tn_skX3CdE7PAVNyp0oisYibH7Xss") ]
        , url = roomsApiUrl ++ "/" ++ String.fromInt roomId
        , body = Http.emptyBody
        , expect = Http.expectJson GotRooms (list roomDecoder)
        , timeout = Nothing
        , tracker = Nothing
        }

init : () -> (Model , Cmd Msg)
init () =
        ( initialModel, initialCmd)

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
