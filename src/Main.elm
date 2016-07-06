module Main exposing (..)

import Navigation
import Players.Main as Players
import Players.EditView as EditV
import Routing exposing (Route(..))
import Html exposing (Html, div, text)
import Html.App


-- MAIN


main : Program Never
main =
    Navigation.program Routing.parser
        { init = init
        , view = view
        , update = update
        , urlUpdate = urlUpdate
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { players : List Players.Player
    , route : Routing.Route
    }


initialModel : Routing.Route -> Model
initialModel route =
    { players = []
    , route = route
    }


init : Result String Routing.Route -> ( Model, Cmd Msg )
init result =
    ( Routing.routeFromResult result |> initialModel, Cmd.map PlayersMsg Players.fetchAll )



-- MESSAGES


type Msg
    = PlayersMsg Players.Msg



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ page model ]


page : Model -> Html Msg
page model =
    case model.route of
        PlayersRoute ->
            Html.App.map PlayersMsg (Players.view model.players)

        PlayerRoute id ->
            playerEditPage model id

        NotFoundRoute ->
            notFoundView


playerEditPage : Model -> Players.PlayerId -> Html Msg
playerEditPage model playerId =
    let
        maybePlayer =
            model.players
                |> List.filter (\player -> player.id == playerId)
                |> List.head
    in
        case maybePlayer of
            Just player ->
                Html.App.map PlayersMsg (EditV.view player)

            Nothing ->
                notFoundView


notFoundView : Html msg
notFoundView =
    div []
        [ text "Not found"
        ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PlayersMsg subMsg ->
            let
                ( updatedPlayers, cmd ) =
                    Players.update subMsg model.players
            in
                ( { model | players = updatedPlayers }, Cmd.map PlayersMsg cmd )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- ROUTE


urlUpdate : Result String Route -> Model -> ( Model, Cmd Msg )
urlUpdate result model =
    let
        currentRoute =
            Routing.routeFromResult result
    in
        ( { model | route = currentRoute }, Cmd.none )
