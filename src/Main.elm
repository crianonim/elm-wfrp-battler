module Main exposing (main)

import Array exposing (Array)
import Browser
import Character exposing (Character, Team(..), listStillToAct)
import Dice
import Html exposing (Attribute, Html, button, div, option, p, select, text)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick, onInput)
import Platform.Cmd exposing (Cmd)
import Random
import Utils


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


initialCharacters : Array Character
initialCharacters =
    List.map Character.generateCharacterWithTeam [ ( "Goblin", Home ), ( "Wolf", Home ), ( "Human", NoTeam ), ( "Human", Home ), ( "Ogre", Home ), ( "Goblin", Away ), ( "Dwarf", Away ), ( "Ogre", Away ) ]
        |> Array.fromList
        |> Array.indexedMap (\i c -> { c | name = c.name ++ "#" ++ String.fromInt i, id = i })


init : () -> ( Model, Cmd Msg )
init _ =
    ( { result = 0
      , turn = 1
      , phase = InBattle
      , characters = initialCharacters
      , defender = 1
      , currentCharacter =
            Character.listStillToAct (Array.filter Character.isAlive initialCharacters |> Array.toList)
                |> List.head
                |> Maybe.withDefault Character.badCharacter
                |> .id
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
                    Character.getCharacter model.currentCharacter model.characters

                def =
                    Character.getCharacter model.defender model.characters

                ( newDef, newLog ) =
                    Character.attackCharacter d100 att def
            in
            ( { model
                | characters =
                    Character.updateCharacter newDef model.characters
                , log = newLog :: model.log
              }
                |> nextCharacter
            , Cmd.none
            )

        NextCharacter ->
            ( nextCharacter model, Cmd.none )

        AttackClick ->
            ( model, Random.generate Attack (Random.pair Dice.d100Generator Dice.d100Generator) )

        ChooseDefendant x ->
            case String.toInt x of
                Just xx ->
                    ( { model | defender = xx }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        SelectTeamForCharacter id t ->
            let
                team =
                    Character.stringToTeam t
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

        CommitTeamSelection ->
            ( { model | phase = InBattle }, Cmd.none )

        ClickedCharacter id ->
            let
                chosenCharacter =
                    Character.getCharacter id model.characters

                currentCharacter =
                    Character.getCharacter model.currentCharacter model.characters
            in
            if Character.isAttackable currentCharacter chosenCharacter then
                update AttackClick { model | defender = id }

            else
                ( model, Cmd.none )


nextCharacter : Model -> Model
nextCharacter model =
    let
        ( homeCount, awayCount ) =
            Character.countTeamCharactersActive model.characters

        currentCharacter =
            Character.getCharacter model.currentCharacter model.characters

        modelAfterActed =
            { model | characters = Character.updateCharacter { currentCharacter | acted = True } model.characters }

        stillToAct =
            Character.listStillToAct (Array.filter Character.isInBattle modelAfterActed.characters |> Array.toList)

        hasActed =
            Character.listActed (Array.filter Character.isInBattle modelAfterActed.characters |> Array.toList)

        turnFinished =
            List.length stillToAct == 0

        newCurrentIndex =
            (if turnFinished then
                hasActed

             else
                stillToAct
            )
                |> List.head
                |> Maybe.withDefault Character.badCharacter
                |> .id

        turn =
            if turnFinished then
                modelAfterActed.turn + 1

            else
                modelAfterActed.turn

        characters =
            if turnFinished then
                Array.map (\c -> { c | acted = False }) modelAfterActed.characters

            else
                modelAfterActed.characters

        newModel =
            { modelAfterActed | currentCharacter = newCurrentIndex, characters = characters, turn = turn }
    in
    (case homeCount * awayCount > 0 of
        False ->
            { model | phase = BattleFinished }

        True ->
            newModel
    )
        |> characterTurnStart


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
    , turn : Int
    , phase : GamePhase
    , characters : Array Character
    , defender : Int
    , log : List String
    , currentCharacter : Int
    }


type Msg
    = GotDice (List Int)
    | Attack ( Int, Int )
    | AttackClick
    | NextCharacter
    | ChooseDefendant String
    | SelectTeamForCharacter Int String
    | CommitTeamSelection
    | ClickedCharacter Int


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
        [ div [] [ "Turn: " ++ (String.fromInt model.turn) |> text ]
        , div []
            [ case model.phase of
                BattleFinished ->
                    viewBattleFinished model

                InBattle ->
                    viewInBattle model

                TeamSelection ->
                    viewTeamSelection model
            ]
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
                        , select [ onInput (SelectTeamForCharacter c.id) ]
                            (List.map
                                (\t ->
                                    option
                                        [ Html.Attributes.value (Character.teamToString t)
                                        , Html.Attributes.selected (c.team == t)
                                        ]
                                        [ text (Character.teamToString t) ]
                                )
                                [ NoTeam, Home, Away ]
                            )
                        ]
                )
                model.characters
                |> Array.toList
            )
        , button [ onClick CommitTeamSelection ] [ text "Start" ]
        ]


viewInBattle : Model -> Html Msg
viewInBattle model =
    div []
        [ p [] [ text "Battle is on!" ]
        , div [ class "sides-container" ]
            [ div [ class "side-home" ] (List.map (viewCharacter model.currentCharacter) (Character.charactersInTeam Home (Array.toList model.characters)))
            , div [ class "sides-middle" ]
                [ div [ class "actions" ]
                    [ button [ onClick AttackClick ] [ text "Attack" ]
                    , div [] [ viewListAliveCharacters model ]
                    , button [ onClick NextCharacter ] [ text "Next" ]
                    ]
                , viewLog model.log
                ]
            , div [ class "side-away" ] (List.map (viewCharacter model.currentCharacter) (Character.charactersInTeam Away (Array.toList model.characters)))
            ]
        
        , viewBattleQueue model.characters model.currentCharacter
        ]


viewBattleQueue : Array Character -> Int -> Html Msg
viewBattleQueue characters currentCharacterId =
    div [ class "battle-queue" ]
        (List.concat
            [ List.map (viewCharacter currentCharacterId)
                (Array.toList characters
                    |> List.filter Character.isInBattle
                    |> Character.listStillToAct
                )
            , [ div [ Html.Attributes.style "width" "100px" ] [ text "New Turn" ] ]
            , List.map (viewCharacter currentCharacterId)
                (Array.toList characters
                    |> List.filter Character.isInBattle
                    |> Character.listActed
                )
            ]
        )


viewLog : List String -> Html msg
viewLog log =
    div [ class "log" ] (List.map viewLogLine log)


viewBattleFinished : Model -> Html msg
viewBattleFinished model =
    div []
        [ p [] [ text "Battle finished" ]
        , p []
            [ text
                ("Won "
                    ++ (let
                            ( homeCount, awayCount ) =
                                Character.countTeamCharactersActive model.characters
                        in
                        if homeCount > awayCount then
                            " Home "

                        else
                            " Away "
                       )
                    ++ " team."
                )
            ]
        ]


viewCharacter : Int -> Character.Character -> Html Msg
viewCharacter currentCharacterId character =
    div
        [ classList
            [ ( "character-block", True )
            , ( "dead", not <| Character.isAlive character )
            , ( "current-character", character.id == currentCharacterId )
            , ( "acted", character.acted )
            , ( "away-fg", character.team == Away )
            , ( "home-fg", character.team == Home )
            ]
        , onClick (ClickedCharacter character.id)
        ]
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
