{- Room View -}


module Room exposing (Model, Msg, init, update, view)

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
    = GotRoom (Result Http.Error  Room)


view : Model -> Html Msg
view model =
        case model.status of
            Loaded room ->
                viewLoaded room

            Loading ->
                 div [ class "loadingSpinner" ]
                    [ span [ class "loadingSpinner-inner" ] []
                    , span [ class "loadingSpinner-inner" ] []
                    , span [ class "loadingSpinner-inner" ] []
                    , span [ class "loadingSpinner-inner" ] []
                    ]
                

            Errored errorMessage ->
                 text ("Error: " ++ errorMessage) 


viewLoaded : Room -> Html Msg
viewLoaded room =
    div [] [
    div [ class "flex-grow bg-gray" ] [ h4 [ class "text-big text-white m-none" ] [ text ( "Room " ++ String.fromInt room.id ) ] ] 
    , label [ class "badge" ] [ text "Description"]
    , br [] []
    , p [ class "text-medium"] [ text ( room.hab_descripcion ) ]
    , label [ class "badge" ] [ text "Room Number"]
    , br [] []
    , p [class "text-medium"] [ text ( String.fromInt room.hab_numero ) ]
    , label [ class "badge" ] [ text "Capacity"]
    , br [] []
    , p [class "text-medium"] [ text ( String.fromInt room.hab_capacidad )]
    , label [ class "badge" ] [ text "Hotel"]
    , br [] []
    , p [ class "text-medium" ] [ text  (String.fromInt room.fk_hot_codigo)]
    , label [ class "badge" ] [ text "Price"]
    , br [] []
    , p [ class "text-medium" ] [ text (String.fromFloat room.hab_precio)]
        

    {--, h3 [] [ text "Room Size: " ]
      - , div [ id "choose-size" ] (List.map viewSizeChooser [ Small, Medium, Large ]) --}
    {-- [>, div [ id "room", class (sizeToString chosenSize) ] (List.map (viewRoom selectedUrl) room) <] --}
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
        , td [] [ button [ class "button button--small button--green"] [ text "Book!" ], button [ class "button button--small button--primary"] [ text "Modify!" ] ]
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
    = Loaded Room
    | Loading
    | Errored String


type alias Model =
    { status : Status
    }


initialModel : Model
initialModel =
    { status = Loading
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotRoom (Ok room) ->
                    ( { model | status = Loaded room }, Cmd.none )
        GotRoom (Err _) ->
            ( { model | status = Errored "An Unknown Error Ocurred. Please Try Again Later" }, Cmd.none )


{--Initial Command for retrieving information from server, as an HTTP GET request  --}

roomApiUrl : String
roomApiUrl = 
        C.apiUrl ++ "rooms/" ++"2"

initialCmd : Cmd Msg
initialCmd =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" ("Bearer " ++ "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJqd3QiOjZ9.eVgBafOwYssLx9tn_skX3CdE7PAVNyp0oisYibH7Xss") ]
        , url = roomApiUrl
        , body = Http.emptyBody
        , expect = Http.expectJson GotRoom roomDecoder
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
