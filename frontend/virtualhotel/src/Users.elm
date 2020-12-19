{- Users View -}


module Users exposing (Model, Msg, init, update, view)

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
import Model exposing (User)


-- Selected User record constant
-- Msg type. What we expect to get from our REST API


type Msg
    = GotUsers (Result Http.Error (List User))
    | DeleteUser Int


view : Model -> Html Msg
view model =
    div [ class "content" ] <|
        case model.status of
            Loaded users ->
                viewLoaded users

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


viewLoaded : List User -> List (Html Msg)
viewLoaded users =
    [ h3 [ class "text-huge text-black text-withSubtitle" ] [ text "Virtual Hotel" ]
    , h4 [ class "text-big text-gray m-none" ] [ text "Users" ] 
    , table [ class "table table--responsive" ]
        [ thead []
            [ tr []
                [ th [] [ text "Id" ], th [] [ text "Name" ], th [] [ text "User Identification" ], th [] [ text "Email" ], th [] [ text "Birth Date" ], th [] [ text "Phone" ], th [] [ text "Status" ] ]
            ]
        , tbody [] (List.map viewUser  users )
        ]

    {--, h3 [] [ text "User Size: " ]
      - , div [ id "choose-size" ] (List.map viewSizeChooser [ Small, Medium, Large ]) --}
    {--, div [ id "users", class (sizeToString chosenSize) ] (List.map (viewUser selectedUrl) users) --}
    {--, img [ class "large", src (urlPrefix ++ "large/" ++ selectedUrl) ] [] --}
    ]


viewUser : User -> Html Msg
viewUser user =
    tr []
        [ td [] [ text (String.fromInt user.id) ]
        , td [] [ text user.usu_nombre ]
        , td [] [ text user.usu_identificacion ]
        , td [] [ text user.usu_email ]
        , td [] [ text user.usu_fec_nac ]
        , td [] [ text user.usu_telefono]
        , td [] [ text user.usu_estado ]
        , td [] [ button [ class "button button--small button--green"] [ text "View!" ], button [ onClick (DeleteUser user.id), class "button button--small button--primary"] [ text "Remove!" ] ]
        ]




-- User decoder fromJSON


userDecoder : Decoder User
userDecoder =
    succeed User
        |> required "usu_nombre" string 
        |> required "usu_identificacion" string 
        |> required "usu_password" string 
        |> required "usu_email" string 
        |> required "usu_estado" string 
        |> required "usu_fec_nac" string 
        |> required "usu_telefono" string 
        |> required "id" int 
         



type Status
    = Loaded (List User)
    | Loading
    | Errored String


type alias Model =
    { status : Status
    , selectedUser : Maybe Int
    }


initialModel : Model
initialModel =
    { status = Loading
    , selectedUser = Nothing
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of

        DeleteUser userId ->
            ( model, deleteCmd userId  )

        GotUsers (Ok users) ->
            case users of
                first :: rest ->
                    ( { model | status = Loaded users }, Cmd.none )

                [] ->
                    ( { model | status = Errored "Ningun Usuario Encontrado" }, Cmd.none )

        GotUsers (Err _) ->
            ( { model | status = Errored "An Unknown Error Ocurred. Please Try Again Later" }, Cmd.none )


{--Initial Command for retrieving information from server, as an HTTP GET request  --}

usersApiUrl : String
usersApiUrl = 
        C.apiUrl ++ "users"

{-- PATCH / DELETE COMMAND  --}

deleteCmd : Int -> Cmd Msg
deleteCmd userId =
    Http.request
        { method = "PATCH"
        , headers = [ Http.header "Authorization" ("Bearer " ++ "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJqd3QiOjZ9.eVgBafOwYssLx9tn_skX3CdE7PAVNyp0oisYibH7Xss") ]
        , url = usersApiUrl ++ "/" ++ String.fromInt userId
        , body = Http.emptyBody
        , expect = Http.expectJson GotUsers (list userDecoder)
        , timeout = Nothing
        , tracker = Nothing
        }

initialCmd : Cmd Msg
initialCmd =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" ("Bearer " ++ "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJqd3QiOjZ9.eVgBafOwYssLx9tn_skX3CdE7PAVNyp0oisYibH7Xss") ]
        , url = usersApiUrl
        , body = Http.emptyBody
        , expect = Http.expectJson GotUsers (list userDecoder)
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
