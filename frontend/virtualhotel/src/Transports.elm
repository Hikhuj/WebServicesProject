{- Transports View -}


module Transports exposing (Model, Msg, init, update, view)

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
import Model exposing (Transport)


-- Selected Transport record constant
-- Msg type. What we expect to get from our REST API


type Msg
    = GotTransports (Result Http.Error (List Transport))


view : Model -> Html Msg
view model =
    div [ class "content" ] <|
        case model.status of
            Loaded transports ->
                viewLoaded transports

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


viewLoaded : List Transport -> List (Html Msg)
viewLoaded transports =
    [ h3 [ class "text-huge text-black text-withSubtitle" ] [ text "Virtual Hotel" ]
    , h4 [ class "text-big text-gray m-none" ] [ text "Transports" ] 
    , table [ class "table table--responsive" ]
        [ thead []
            [ tr []
                [ th [] [ text "Id" ], th [] [ text "Name" ], th [] [ text "Price" ], th [] [ text "Status" ]  ]
            ]
        , tbody [] (List.map viewTransport  transports )
        ]

    {--, h3 [] [ text "Transport Size: " ]
      - , div [ id "choose-size" ] (List.map viewSizeChooser [ Small, Medium, Large ]) --}
    {--, div [ id "transports", class (sizeToString chosenSize) ] (List.map (viewTransport selectedUrl) transports) --}
    {--, img [ class "large", src (urlPrefix ++ "large/" ++ selectedUrl) ] [] --}
    ]


viewTransport : Transport -> Html Msg
viewTransport transport =
    tr []
        [ td [] [ text (String.fromInt transport.id) ]
        , td [] [ text transport.tra_descripcion ]
        , td [] [ text (String.fromFloat transport.tra_precio) ]
        , td [] [ text transport.tra_estado ]
        , td [] [ button [ class "button button--small button--green"] [ text "View!" ], button [ class "button button--small button--primary"] [ text "Remove!" ] ]
        ]




-- Transport decoder fromJSON


transportDecoder : Decoder Transport
transportDecoder =
    succeed Transport
        |> required "fk_tip_transporte" int
        |> required "tra_descripcion"  string 
        |> required "tra_precio"  float
        |> required "tra_estado"  string 
        |> required "id"  int 
         


type Status
    = Loaded (List Transport)
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
        GotTransports (Ok transports) ->
            case transports of
                first :: rest ->
                    ( { model | status = Loaded transports }, Cmd.none )

                [] ->
                    ( { model | status = Errored "Ninguna Habitacion Encontrada" }, Cmd.none )

        GotTransports (Err _) ->
            ( { model | status = Errored "An Unknown Error Ocurred. Please Try Again Later" }, Cmd.none )


{--Initial Command for retrieving information from server, as an HTTP GET request  --}

transportsApiUrl : String
transportsApiUrl = 
        C.apiUrl ++ "transports"

initialCmd : Cmd Msg
initialCmd =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" ("Bearer " ++ "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJqd3QiOjZ9.eVgBafOwYssLx9tn_skX3CdE7PAVNyp0oisYibH7Xss") ]
        , url = transportsApiUrl
        , body = Http.emptyBody
        , expect = Http.expectJson GotTransports (list transportDecoder)
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
