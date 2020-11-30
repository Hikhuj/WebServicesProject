module Main exposing (main)

import Browser exposing (Document)
import Html exposing (Html, a, footer, h1, li, nav, text, ul)
import Html.Attributes exposing (classList, href)


type alias Model =
    { page : Page }


type Page
    = Rooms
    | ViewRoom
    | ModifyRoom
    | CreateRoom
    | Reservations


view : Model -> Document Msg
view model =
    let
        content =
            text "Welcome!"
    in
    { title = "Virtual Hotel"
    , body =
        [ viewHeader model.page
        , content
        , viewFooter
        ]
    }



-- Header View


viewHeader : Page -> Html Msg
viewHeader page =
    let
        logo =
            h1 [] [ text "Virtual Hotel" ]

        links =
            ul []
                [ navLink Rooms { url = "rooms", caption = "Rooms" }
                , navLink Reservations { url = "reservations", caption = "Reservations" }
                ]

        navLink : Page -> { url : String, caption : String } -> Html msg
        navLink targetPage ({ url, caption } as config) =
            li [ classList [ ( "tabs-item is-selected", page == targetPage ) ] ]
                [ a [ href config.url ] [ text config.caption ] ]
    in
    nav [] [ logo, links ]



-- Footer View


viewFooter : Html Msg
viewFooter =
    footer [] [ text "Copyleft CC 2020-2021" ]


type Msg
    = NothingYet



-- Update Views


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> ( { page = Reservations }, Cmd.none )
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
