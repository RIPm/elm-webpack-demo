module Players.Main exposing (..)

import Navigation
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing ((:=))
import Json.Encode as Encode
import Task


-- MODEL


type alias PlayerId =
    Int


type alias Player =
    { id : PlayerId
    , name : String
    , level : Int
    }


initPlayer : Player
initPlayer =
    { id = 0
    , name = ""
    , level = 1
    }



-- MESSAGES


type Msg
    = FetchAllDone (List Player)
    | FetchAllFail Http.Error
    | ShowPlayers
    | ShowPlayer PlayerId
    | ChangeLevel PlayerId Int
    | SaveSuccess Player
    | SaveFail Http.Error



-- VIEW


view : List Player -> Html Msg
view players =
    div []
        [ nav players
        , list players
        ]


nav : List Player -> Html Msg
nav players =
    div [ class "clearfix mb2 white bg-black" ]
        [ div [ class "left p2" ] [ text "Players" ] ]


list : List Player -> Html Msg
list players =
    div [ class "p2" ]
        [ table []
            [ thead []
                [ tr []
                    [ th [] [ text "Id" ]
                    , th [] [ text "Name" ]
                    , th [] [ text "Level" ]
                    , th [] [ text "Actions" ]
                    ]
                ]
            , tbody [] (List.map playerRow players)
            ]
        ]


playerRow : Player -> Html Msg
playerRow player =
    tr []
        [ td [] [ text (toString player.id) ]
        , td [] [ text player.name ]
        , td [] [ text (toString player.level) ]
        , td [] [ editBtn player ]
        ]


editBtn : Player -> Html.Html Msg
editBtn player =
    button
        [ class "btn regular"
        , onClick (ShowPlayer player.id)
        ]
        [ i [ class "fa fa-pencil mr1" ] []
        , text "Edit"
        ]



-- UPDATE


update : Msg -> List Player -> ( List Player, Cmd Msg )
update message players =
    case message of
        FetchAllDone newPlayers ->
            ( newPlayers, Cmd.none )

        FetchAllFail error ->
            ( players, Cmd.none )

        ShowPlayers ->
            ( players, Navigation.modifyUrl "#players" )

        ShowPlayer id ->
            ( players, Navigation.modifyUrl ("#players/" ++ (toString id)) )

        ChangeLevel id howMuch ->
            ( players, players |> changeLevelCommands id howMuch |> Cmd.batch )

        SaveSuccess updatedPlayer ->
            ( players |> updatePlayer updatedPlayer, Cmd.none )

        SaveFail error ->
            ( players, Cmd.none )


changeLevelCommands : PlayerId -> Int -> List Player -> List (Cmd Msg)
changeLevelCommands playerId howMuch =
    let
        cmdForPlayer existingPlayer =
            if existingPlayer.id == playerId then
                save { existingPlayer | level = existingPlayer.level + howMuch }
            else
                Cmd.none
    in
        List.map cmdForPlayer


updatePlayer : Player -> List Player -> List Player
updatePlayer updatedPlayer =
    let
        select existingPlayer =
            if existingPlayer.id == updatedPlayer.id then
                updatedPlayer
            else
                existingPlayer
    in
        List.map select



-- HTTP


fetchAllUrl : String
fetchAllUrl =
    "http://localhost:4000/players"


fetchAll : Cmd Msg
fetchAll =
    Http.get collectionDecoder fetchAllUrl
        |> Task.perform FetchAllFail FetchAllDone


save : Player -> Cmd Msg
save player =
    saveTask player
        |> Task.perform SaveFail SaveSuccess


saveTask : Player -> Task.Task Http.Error Player
saveTask player =
    let
        body =
            memberEncoded player
                |> Encode.encode 0
                |> Http.string

        config =
            { verb = "PUT"
            , headers = [ ( "Content-Type", "application/json" ) ]
            , url = fetchAllUrl ++ "/" ++ (toString player.id)
            , body = body
            }
    in
        Http.send Http.defaultSettings config
            |> Http.fromJson memberDecoder


memberEncoded : Player -> Encode.Value
memberEncoded player =
    let
        list =
            [ ( "id", Encode.int player.id )
            , ( "name", Encode.string player.name )
            , ( "level", Encode.int player.level )
            ]
    in
        list
            |> Encode.object


collectionDecoder : Decode.Decoder (List Player)
collectionDecoder =
    Decode.list memberDecoder


memberDecoder : Decode.Decoder Player
memberDecoder =
    Decode.object3 Player
        ("id" := Decode.int)
        ("name" := Decode.string)
        ("level" := Decode.int)
