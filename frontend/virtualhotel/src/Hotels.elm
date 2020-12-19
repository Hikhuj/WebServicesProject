{- Hotels View -}


module Hotels exposing (Model, Msg, init, update, view)

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
import Model exposing (Hotel)


-- Selected Hotel record constant
-- Msg type. What we expect to get from our REST API


type Msg
    = GotHotels (Result Http.Error (List Hotel))
    | DeleteHotel Int


view : Model -> Html Msg
view model =
    div [ class "content" ] <|
        case model.status of
            Loaded hotels ->
                viewLoaded hotels

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


viewLoaded : List Hotel -> List (Html Msg)
viewLoaded hotels =
    [ h3 [ class "text-huge text-black text-withSubtitle" ] [ text "Virtual Hotel" ]
    , h4 [ class "text-big text-gray m-none" ] [ text "Hotels" ] 
    , table [ class "table table--responsive" ]
        [ thead []
            [ tr []
                [ th [] [ text "Id" ], th [] [ text "Name" ], th [] [ text "Phone" ], th [] [ text "Email" ], th [] [ text "Address" ], th [] [ text "Category" ], th [] [ text "Status" ] ]
            ]
        , tbody [] (List.map viewHotel  hotels )
        ]

    {--, h3 [] [ text "Hotel Size: " ]
      - , div [ id "choose-size" ] (List.map viewSizeChooser [ Small, Medium, Large ]) --}
    {--, div [ id "hotels", class (sizeToString chosenSize) ] (List.map (viewHotel selectedUrl) hotels) --}
    {--, img [ class "large", src (urlPrefix ++ "large/" ++ selectedUrl) ] [] --}
    ]


viewHotel : Hotel -> Html Msg
viewHotel hotel =
    tr []
        [ td [] [ text (String.fromInt hotel.id) ]
        , td [] [ text hotel.hot_nombre ]
        , td [] [ text hotel.hot_telefono ]
        , td [] [ text hotel.hot_email ]
        , td [] [ text hotel.hot_direccion ]
        , td [] [ text hotel.hot_categoria ]
        , td [] [ text hotel.hot_estado ]
        , td [] [ button [ class "button button--small button--green"] [ text "View!" ], button [ onClick (DeleteHotel hotel.id), class "button button--small button--primary"] [ text "Remove!" ] ]
        ]




-- Hotel decoder fromJSON


hotelDecoder : Decoder Hotel
hotelDecoder =
    succeed Hotel
        |> required "hot_nombre" string 
        |> required "hot_telefono" string 
        |> required "hot_email" string 
        |> required "hot_direccion" string 
        |> required "hot_categoria" string 
        |> required "hot_estado" string 
        |> required "id" int 
         



type Status
    = Loaded (List Hotel)
    | Loading
    | Errored String


type alias Model =
    { status : Status
    , selectedHotel : Maybe Int
    }


initialModel : Model
initialModel =
    { status = Loading
    , selectedHotel = Nothing
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of

        DeleteHotel hotelId ->
            ( model, deleteCmd hotelId  )


        GotHotels (Ok hotels) ->
            case hotels of
                first :: rest ->
                    ( { model | status = Loaded hotels }, Cmd.none )

                [] ->
                    ( { model | status = Errored "Ninguna Habitacion Encontrada" }, Cmd.none )

        GotHotels (Err _) ->
            ( { model | status = Errored "An Unknown Error Ocurred. Please Try Again Later" }, Cmd.none )


{--Initial Command for retrieving information from server, as an HTTP GET request  --}

hotelsApiUrl : String
hotelsApiUrl = 
        C.apiUrl ++ "hotels"


{-- PATCH / DELETE COMMAND  --}

deleteCmd : Int -> Cmd Msg
deleteCmd hotelId =
    Http.request
        { method = "PATCH"
        , headers = [ Http.header "Authorization" ("Bearer " ++ "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJqd3QiOjZ9.eVgBafOwYssLx9tn_skX3CdE7PAVNyp0oisYibH7Xss") ]
        , url = hotelsApiUrl ++ "/" ++ String.fromInt hotelId
        , body = Http.emptyBody
        , expect = Http.expectJson GotHotels (list hotelDecoder)
        , timeout = Nothing
        , tracker = Nothing
        }

initialCmd : Cmd Msg
initialCmd =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" ("Bearer " ++ "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJqd3QiOjZ9.eVgBafOwYssLx9tn_skX3CdE7PAVNyp0oisYibH7Xss") ]
        , url = hotelsApiUrl
        , body = Http.emptyBody
        , expect = Http.expectJson GotHotels (list hotelDecoder)
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
