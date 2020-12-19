{- TransportTypes View -}


module TransportTypes exposing (Model, Msg, init, update, view)

-- Browser elements and sandbox
-- Html
-- Html Attributes
-- Html Events
-- Random library
-- JSON Decoders (Official and Third Party, for large decodings)

import Browser
import Constants as C
import Html exposing (..)
import Html.Attributes exposing (class, classList, id, name, src, title, type_, placeholder, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (Decoder, bool, float, int, list, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)
import Model exposing (TransportType, encodeTransportType)
import Json.Encode as Encode


-- Selected TransportType record constant
-- Msg type. What we expect to get from our REST API


type Msg
    = GotTransportTypes (Result Http.Error (List TransportType))
    | DeleteTransportType Int
    | CreateTransportType
    | TransportTypeDescription String


view : Model -> Html Msg
view model =
    div []
        [ viewPostForm model
        , div [ class "content" ] <|
            case model.status of
                Loaded transportTypes ->
                    viewLoaded transportTypes

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
        ]


viewPostForm : Model -> Html Msg
viewPostForm model =
    div [ class "container" ] [
            label [ class "label" ] [ text "Name" ]
            , div [class "input input-fullWidth"] [
                    viewInput "text" "Transport Type.." model.transportTypeDescription TransportTypeDescription
                    ]
                    , button [ onClick (CreateTransportType ) , class "button button--small button--green" ] [ text "Create!" ]
            ]

viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput ty plc val toMsg =
  input [ type_ ty, placeholder plc, value val, onInput toMsg ] []

viewLoaded : List TransportType -> List (Html Msg)
viewLoaded transportTypes =
    [ h3 [ class "text-huge text-black text-withSubtitle" ] [ text "Virtual Hotel" ]
    , h4 [ class "text-big text-gray m-none" ] [ text "TransportTypes" ]
    , table [ class "table table--responsive" ]
        [ thead []
            [ tr []
                [ th [] [ text "Id" ], th [] [ text "Name" ], th [] [ text "Status" ] ]
            ]
        , tbody [] (List.map viewTransportType transportTypes)
        ]

    {--, h3 [] [ text "TransportType Size: " ]
      - , div [ id "choose-size" ] (List.map viewSizeChooser [ Small, Medium, Large ]) --}
    {--, div [ id "transportTypes", class (sizeToString chosenSize) ] (List.map (viewTransportType selectedUrl) transportTypes) --}
    {--, img [ class "large", src (urlPrefix ++ "large/" ++ selectedUrl) ] [] --}
    ]


viewTransportType : TransportType -> Html Msg
viewTransportType transportType =
    tr []
        [ td [] [ text (String.fromInt transportType.id) ]
        , td [] [ text transportType.tip_descripcion ]
        , td [] [ text transportType.tip_estado ]
        , td [] [ button [ class "button button--small button--green" ] [ text "View!" ], button [ onClick (DeleteTransportType transportType.id), class "button button--small button--primary" ] [ text "Remove!" ] ]
        ]



-- TransportType decoder fromJSON


transportTypeDecoder : Decoder TransportType
transportTypeDecoder =
    succeed TransportType
        |> required "tip_descripcion" string
        |> required "tip_estado" string
        |> required "id" int


type Status
    = Loaded (List TransportType)
    | Loading
    | Errored String


type alias Model =
    { status : Status
     , selectedTransportType : Maybe Int
     , transportTypeDescription : String
    }


initialModel : Model
initialModel =
    { status = Loading
    , selectedTransportType = Nothing
    , transportTypeDescription = ""
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of

        TransportTypeDescription newTransportTypeDescription ->
            ( { model | transportTypeDescription = newTransportTypeDescription }, Cmd.none)


        CreateTransportType ->
            ( model, postCmd model )


        DeleteTransportType transportTypeId ->
            ( model, deleteCmd transportTypeId )

        GotTransportTypes (Ok transportTypes) ->
            case transportTypes of
                first :: rest ->
                    ( { model | status = Loaded transportTypes }, Cmd.none )

                [] ->
                    ( { model | status = Errored "Ningun Tipo de Transporte Encontrado" }, Cmd.none )

        GotTransportTypes (Err _) ->
            ( { model | status = Errored "An Unknown Error Ocurred. Please Try Again Later" }, Cmd.none )



{--Initial Command for retrieving information from server, as an HTTP GET request  --}


transportTypesApiUrl : String
transportTypesApiUrl =
    C.apiUrl ++ "types/transports"


initialCmd : Cmd Msg
initialCmd =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" ("Bearer " ++ "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJqd3QiOjZ9.eVgBafOwYssLx9tn_skX3CdE7PAVNyp0oisYibH7Xss") ]
        , url = transportTypesApiUrl
        , body = Http.emptyBody
        , expect = Http.expectJson GotTransportTypes (list transportTypeDecoder)
        , timeout = Nothing
        , tracker = Nothing
        }


{-- CREATE / POST  --}
postCmd : Model -> Cmd Msg
postCmd model =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" ("Bearer " ++ "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJqd3QiOjZ9.eVgBafOwYssLx9tn_skX3CdE7PAVNyp0oisYibH7Xss") ]
        , url = transportTypesApiUrl
        , body = Http.jsonBody (encodeTransportType model.transportTypeDescription)
        , expect = Http.expectJson GotTransportTypes (list transportTypeDecoder)
        , timeout = Nothing
        , tracker = Nothing
        }



{--PATCH / DELETE COMMAND  --}


deleteCmd : Int -> Cmd Msg
deleteCmd transportTypeId =
    Http.request
        { method = "PATCH"
        , headers = [ Http.header "Authorization" ("Bearer " ++ "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJqd3QiOjZ9.eVgBafOwYssLx9tn_skX3CdE7PAVNyp0oisYibH7Xss") ]
        , url = transportTypesApiUrl ++ "/" ++ String.fromInt transportTypeId
        , body = Http.emptyBody
        , expect = Http.expectJson GotTransportTypes (list transportTypeDecoder)
        , timeout = Nothing
        , tracker = Nothing
        }


init : () -> ( Model, Cmd Msg )
init () =
    ( initialModel, initialCmd )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
