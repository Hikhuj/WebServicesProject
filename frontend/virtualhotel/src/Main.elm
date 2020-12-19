module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Html exposing (Html, a, footer, h1, li, nav, text, ul)
import Html.Attributes exposing (classList, href)
import Html.Events exposing (onClick)
import Html.Lazy exposing (lazy)
import Reservations
import Rooms
import TransportTypes
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, s, string)


type alias Model =
    { page : Page
    , key : Nav.Key
    }


type Page
    = RoomsPage Rooms.Model
    | RoomPage String
    | ModifyRoomPage String
    | CreateRoomPage
    | ReservationsPage Reservations.Model
    | TransportTypesPage TransportTypes.Model
    | NotFound


type Route
    = Rooms
    | Room
    | CreateRoom
    | Reservations
    | TransportTypes


parser : Parser (Route -> a) a
parser =
    Parser.oneOf
        [ Parser.map Reservations Parser.top
        , Parser.map Rooms (s "rooms")
        , Parser.map Reservations (s "reservations")
        , Parser.map TransportTypes (s "transport-types")

        {--, Parser.map Room (s "rooms" </> Parser.string) --}
        ]


view : Model -> Document Msg
view model =
    let
        content =

            case Debug.log "The Page says: " model.page of

                ReservationsPage reservations ->
                    Reservations.view reservations
                        |> Html.map GotReservationsMsg

                TransportTypesPage reservations ->
                    TransportTypes.view reservations
                        |> Html.map GotTransportTypesMsg

                RoomsPage rooms ->
                    Rooms.view rooms
                        |> Html.map GotRoomsMsg

                NotFound ->
                    h1 [] [ text "Not Found" ]
                _ ->
                    h1 [] [ text "An Unknown Error Has Occurred. Please Contact the Administrator" ]
    in
    { title = "Virtual Hotel"
    , body =
        -- Lazy implementation for viewHeader
        [ lazy viewHeader model.page
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
                , navLink TransportTypes { url = "transport-types", caption = "Transport Types" }
                ]

        navLink : Route -> { url : String, caption : String } -> Html msg
        navLink route { url, caption }  =
            li [ classList [ ( "tabs-item is-selected", isActive { link = route, page = page } ) ] ]
                [ a [ href url ] [ text caption ] ]
    in
    nav [] [ logo, links ]


isActive : { link : Route, page : Page } -> Bool
isActive { link, page } =
    case ( link, page ) of
        ( Rooms, RoomsPage _ ) ->
            True

        ( Rooms, RoomPage _ ) -> True
        ( Rooms, ModifyRoomPage _ ) -> True
        ( Rooms, CreateRoomPage  ) -> True
        ( Rooms, _ ) ->
            False
        ( Reservations , ReservationsPage _ ) -> True
        ( Reservations , _ ) -> False
        ( TransportTypes , TransportTypesPage _ ) -> True
        ( TransportTypes , _ ) -> False
        _ -> False
-- Footer View


viewFooter : Html Msg
viewFooter =
    footer [] [ text "Copyleft CC 2020-2021" ]


type Msg
    = ClickedLink Browser.UrlRequest
    | ChangedUrl Url
    | GotRoomsMsg Rooms.Msg
    | GotReservationsMsg Reservations.Msg
    | GotTransportTypesMsg TransportTypes.Msg



-- Update Views


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of

        ClickedLink urlRequest ->
            case urlRequest of
                Browser.External href ->
                    (model, Nav.load href)

                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

        ChangedUrl url ->
            updateUrl url model

        GotRoomsMsg roomsMsg ->
            case model.page of
                RoomsPage rooms ->
                    toRooms model (Rooms.update roomsMsg rooms)

                NotFound ->
                    ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GotTransportTypesMsg transportTypesMsg ->
            case model.page of
                TransportTypesPage transportTypes ->
                    toTransportTypes model (TransportTypes.update transportTypesMsg transportTypes)

                NotFound ->
                    ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GotReservationsMsg reservationsMsg ->
            case model.page of
                ReservationsPage reservations ->
                    toReservations model (Reservations.update reservationsMsg reservations)

                NotFound ->
                    ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )


-- toRooms view updater


toRooms : Model -> ( Rooms.Model, Cmd Rooms.Msg ) -> ( Model, Cmd Msg )
toRooms model ( rooms, cmd ) =
    ( { model | page = RoomsPage rooms }
    , Cmd.map GotRoomsMsg cmd
    )


toReservations : Model -> ( Reservations.Model, Cmd Reservations.Msg ) -> ( Model, Cmd Msg )
toReservations model ( reservations, cmd ) =
    ( { model | page = ReservationsPage reservations }
    , Cmd.map GotReservationsMsg cmd
    )


toTransportTypes : Model -> ( TransportTypes.Model, Cmd TransportTypes.Msg ) -> ( Model, Cmd Msg )
toTransportTypes model ( transportTypes, cmd ) =
    ( { model | page = TransportTypesPage transportTypes }
    , Cmd.map GotTransportTypesMsg cmd
    )


-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- init


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    updateUrl url { page = NotFound, key = key }



-- Converts Url to Page


updateUrl : Url -> Model -> ( Model, Cmd Msg )
updateUrl url model =
    case Parser.parse parser url of
        Just Rooms ->
            Rooms.init ()
                |> toRooms model

        Just Reservations ->
            Reservations.init ()
                |> toReservations model

        Just TransportTypes ->
            TransportTypes.init ()
                |> toTransportTypes model

        Nothing ->
            ( { model | page = NotFound }, Cmd.none )
        _ ->
            ( { model | page = NotFound }, Cmd.none )


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }
