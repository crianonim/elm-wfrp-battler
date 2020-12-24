module Main exposing (main)

import Array exposing (Array)
import Browser
import Character exposing (Character, Team(..))
import Dice
import Dict
import Html exposing (Attribute, Html, a, button, div, h1, hr, img, input, option, p, pre, select, text)
import Html.Attributes exposing (class, selected, src, style, value)
import Html.Events exposing (onClick, onInput)
import Platform.Cmd exposing (Cmd)
import Random
import Utils
import Html.Attributes exposing (classList)



main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { result = 0
      , phase =  InBattle
      , characters =
            List.map Character.generateCharacterWithTeam [ ("Goblin",Home), ("Wolf",Away), ("Wolf",Home),("Goblin",Home), ("Goblin",Away)]
                |> Array.fromList
                |> Array.indexedMap (\i c -> { c | name = c.name ++ "#" ++ String.fromInt i, id = i })
      , defender = 1
      , currentCharacter = 0
      , log = [ "Started" ]
      }
    , Random.generate GotDice (Dice.rollGenerator 1 100)
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotDice x ->
            ( { model | result = Dice.rollResult x }, Cmd.none )

        Attack d100 ->
            let
                att =
                    Array.get model.currentCharacter model.characters |> Maybe.withDefault Character.badCharacter

                def =
                    Array.get model.defender model.characters |> Maybe.withDefault Character.badCharacter

                ( newDef, newLog ) =
                    Character.attackCharacter d100 att def
            in
            ( { model
                | characters =
                    Array.indexedMap
                        (\i c ->
                            if i == model.defender then
                                newDef

                            else
                                c
                        )
                        model.characters
                , log = newLog :: model.log
              }
                |> nextCharacter
            , Cmd.none
            )

        NextCharacter ->
            ( nextCharacter model, Cmd.none )

        AttackClick ->
            ( model, Random.generate Attack Dice.d100Generator )

        ChooseDefendant x ->
            let
                c =
                    Debug.log ("X" ++ x)

                i =
                    String.toInt x
            in
            case i of
                Just xx ->
                    ( { model | defender = xx }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        SelectTeamForCharacter s ->
            let
                _ = Debug.log "X" s
                parts =
                    String.split "-" s
                        |> Array.fromList

                id = 
                    Array.get 0 parts |> Maybe.withDefault "0" |> String.toInt |> Maybe.withDefault 0

                team =
                    Character.stringToTeam (Array.get 1 parts |> Maybe.withDefault "no")
            in
            ( { model
                | characters =
                    Array.map
                        (\c ->
                            if c.id == id then
                                { c | team = team }

                            else
                                c
                        )
                        model.characters
              }
            , Cmd.none
            )
        CommitTeamSelection -> ({model|phase=InBattle},Cmd.none)


nextCharacter : Model -> Model
nextCharacter model =
   let 
        (homeCount,awayCount) = Character.countTeamCharactersActive model.characters

        newCurrentIndex =
            Utils.incOverflow model.currentCharacter (Array.length model.characters)

        newCurrentCharacter =
            case Array.get newCurrentIndex model.characters of
                Just c ->
                    c

                Maybe.Nothing ->
                    Character.badCharacter

        newModel =
            { model | currentCharacter = newCurrentIndex }
    in
    (case (homeCount*awayCount > 0 ) of
        False ->
            { model | phase = BattleFinished }

        True ->
            case (Character.isAlive newCurrentCharacter && Character.isInTeam newCurrentCharacter) of
                False ->
                    nextCharacter newModel

                True ->
                    newModel
    )
        |> characterTurnStart


firstOpponent : Model -> Int
firstOpponent model =
    Character.listOfOpponents model.characters model.currentCharacter
        |> List.map (\c -> c.id)
        |> List.head
        |> Maybe.withDefault -1
        |> Debug.log "Testing"


characterTurnStart : Model -> Model
characterTurnStart model =
    let
        opponents =
            Character.listOfOpponents model.characters model.currentCharacter

        firstOpponentId =
            Debug.log "x" (List.head (List.map (\c -> c.id) opponents)) |> Maybe.withDefault 0
    in
    { model | defender = firstOpponentId }


type alias Model =
    { result : Int
    , phase : GamePhase
    , characters : Array Character
    , defender : Int
    , log : List String
    , currentCharacter : Int
    }


type Msg
    = GotDice (List Int)
    | Attack Int
    | AttackClick
    | NextCharacter
    | ChooseDefendant String
    | SelectTeamForCharacter String
    | CommitTeamSelection


type GamePhase
    = InBattle
    | BattleFinished
    | TeamSelection



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text (String.fromInt model.result) ]
        
        , div []
            [ case model.phase of
                BattleFinished ->
                    viewBattleFinished model

                InBattle ->
                    viewInBattle model

                TeamSelection ->
                    viewTeamSelection model
            ]
        , div [] (List.map viewLogLine model.log)
        ]


viewTeamSelection : Model -> Html Msg
viewTeamSelection model =
    div []
        [ p [] [ text "Choose teams" ]
        , div []
            (Array.map
                (\c ->
                    div []
                        [ text c.name
                        , select [ onInput SelectTeamForCharacter ]
                            (List.map
                                (\x ->
                                    option
                                        [ Html.Attributes.value (characterToTeamString c x)
                                        , Html.Attributes.selected (c.team == x)
                                        ]
                                        [ text (Character.teamToString x) ]
                                )
                                [ NoTeam, Home, Away ]
                            )
                        ]
                )
                model.characters
                |> Array.toList
            )
        , button [onClick CommitTeamSelection] [text "Start"]
        ]


characterToTeamString : Character -> Team -> String
characterToTeamString c team =
    String.fromInt c.id
        ++ "-"
        ++ Character.teamToString team


viewInBattle : Model -> Html Msg
viewInBattle model =
    div []
        [ p [] [ text "Battle is on!" ]
        , div [class "characters-block"] (List.indexedMap (viewCharacter model) (Array.toList model.characters))
        , button [ onClick AttackClick ] [ text "Attack" ]
        , button [ onClick NextCharacter ] [ text "Next" ]
        , div [ ] [ viewListAliveCharacters model ]
        ]


viewBattleFinished : Model -> Html msg
viewBattleFinished model =
    div []
        [ p [] [ text "Battle finished" ]
        , p [] [ text ("Won " ++ ( 
            let
              (homeCount,awayCount) =Character.countTeamCharactersActive model.characters
             in 
              if (homeCount>awayCount) then " Home " else " Away "
            ) ++ " team.") ]
        ]


viewCharacter : Model -> Int -> Character.Character -> Html msg
viewCharacter model i character =
    div
        ([ 
          classList [
              ("character-block",True)
              ,("dead",not <| Character.isAlive character)
              ,("current-character",i == model.currentCharacter)
              ,( "away-fg", character.team==Away)
              ,( "home-fg", character.team==Home)
          ]
         ]
            
        )
        [ text <| character.name ++ " " ++ String.fromInt character.currentHP ++ "/    " ++ String.fromInt character.baseHP ]




viewLogLine : String -> Html msg
viewLogLine l =
    p [] [ text l ]


viewListAliveCharacters : Model -> Html Msg
viewListAliveCharacters model =
    div []
        [ select [ onInput ChooseDefendant ] (List.map (viewCharacterOption model.defender) (Character.listOfOpponents model.characters model.currentCharacter))
        ]


viewCharacterOption : Int -> Character -> Html msg
viewCharacterOption id c =
    option
        [ Html.Attributes.value (String.fromInt c.id)
        , Html.Attributes.selected (c.id == id)
        ]
        [ text (String.fromInt c.id ++ " " ++ c.name) ]
