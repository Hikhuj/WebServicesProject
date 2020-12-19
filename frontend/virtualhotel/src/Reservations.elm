{- Reservations View -}


module Reservations exposing (Model, Msg, init, update, view)

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
import Model exposing (Reservation)


-- Selected Reservation record constant
-- Msg type. What we expect to get from our REST API


type Msg
    = GotReservations (Result Http.Error (List Reservation))
    | DeleteReservation Int


view : Model -> Html Msg
view model =
    div [ class "content" ] <|
        case model.status of
            Loaded reservations ->
                viewLoaded reservations

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


viewLoaded : List Reservation -> List (Html Msg)
viewLoaded reservations =
    [ h3 [ class "text-huge text-black text-withSubtitle" ] [ text "Virtual Hotel" ]
    , h4 [ class "text-big text-gray m-none" ] [ text "Reservations" ] 
    , table [ class "table table--responsive" ]
        [ thead []
            [ tr []
                [ th [] [ text "Id" ], th [] [ text "Name" ], th [] [ text "Price" ], th [] [ text "Hotel" ], th [] [ text "Reservation Capacity" ], th [] [ text "Status" ] ]
            ]
        , tbody [] (List.map viewReservation  reservations )
        ]

    {--, h3 [] [ text "Reservation Size: " ]
      - , div [ id "choose-size" ] (List.map viewSizeChooser [ Small, Medium, Large ]) --}
    {--, div [ id "reservations", class (sizeToString chosenSize) ] (List.map (viewReservation selectedUrl) rooms) --}
    {--, img [ class "large", src (urlPrefix ++ "large/" ++ selectedUrl) ] [] --}
    ]


viewReservation : Reservation -> Html Msg
viewReservation reservation =
    tr []
        [ td [] [ text (String.fromInt reservation.id) ]
        , td [] [ text  (String.fromInt reservation.fk_usu_codigo) ]
        , td [] [ text  (String.fromInt reservation.fk_hab_codigo) ]
        , td [] [ text  (String.fromInt reservation.fk_tra_codigo) ]
        , td [] [ text reservation.res_fecha_ingreso ]
        , td [] [ text reservation.res_fecha_salida ]
        , td [] [ text reservation.res_estado ]
        , td [] [ button [ class "button button--small button--green"] [ text "View!" ], button [ onClick (DeleteReservation reservation.id) , class "button button--small button--primary"] [ text "Remove!" ] ]
        ]




-- Reservation decoder fromJSON


reservationDecoder : Decoder Reservation
reservationDecoder =
    succeed Reservation
        |> required "fk_usu_codigo" int
        |> required "fk_hab_codigo" int
        |> required "fk_tra_codigo" int
        |> required "res_fecha_ingreso" string 
        |> required "res_fecha_salida" string 
        |> required "res_estado" string 
        |> required "id" int


type Status
    = Loaded (List Reservation)
    | Loading
    | Errored String


type alias Model =
    { status : Status
    , selectedReservation : Maybe Int
    }


initialModel : Model
initialModel =
    { status = Loading
    , selectedReservation = Nothing
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of

        DeleteReservation reservationId ->
            ( model, deleteCmd reservationId  )


        GotReservations (Ok reservations) ->
            case reservations of
                first :: rest ->
                    ( { model | status = Loaded reservations }, Cmd.none )

                [] ->
                    ( { model | status = Errored "Ninguna Reservacion Encontrada" }, Cmd.none )

        GotReservations (Err _) ->
            ( { model | status = Errored "An Unknown Error Ocurred. Please Try Again Later" }, Cmd.none )


{--Initial Command for retrieving information from server, as an HTTP GET request  --}

reservationsApiUrl : String
reservationsApiUrl = 
        C.apiUrl ++ "reservations"

initialCmd : Cmd Msg
initialCmd =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" ("Bearer " ++ "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJqd3QiOjZ9.eVgBafOwYssLx9tn_skX3CdE7PAVNyp0oisYibH7Xss") ]
        , url = reservationsApiUrl
        , body = Http.emptyBody
        , expect = Http.expectJson GotReservations (list reservationDecoder)
        , timeout = Nothing
        , tracker = Nothing
        }

{-- PATCH / DELETE COMMAND  --}

deleteCmd : Int -> Cmd Msg
deleteCmd reservationId =
    Http.request
        { method = "PATCH"
        , headers = [ Http.header "Authorization" ("Bearer " ++ "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJqd3QiOjZ9.eVgBafOwYssLx9tn_skX3CdE7PAVNyp0oisYibH7Xss") ]
        , url = reservationsApiUrl ++ "/" ++ String.fromInt reservationId
        , body = Http.emptyBody
        , expect = Http.expectJson GotReservations (list reservationDecoder)
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
