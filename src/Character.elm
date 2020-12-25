module Character exposing (..)

import Array exposing (Array)



type alias Character =
    { name : String
    , type_ : String
    , baseHP : Int
    , currentHP : Int
    , weaponDmg : Int
    , weaponSkill : Int
    , initiative: Int
    , toughness: Int
    , acted: Bool
    , id : Int
    , team: Team
    }

type Team 
   = Home
   | Away
   | NoTeam

teamToString : Team -> String
teamToString t = 
 case t of
  Home -> "home"
  Away -> "away"
  _ -> "no" 

stringToTeam : String -> Team
stringToTeam s =
 case s of 
  "home" -> Home
  "away" -> Away
  _ -> NoTeam

isInAnyTeam : Character -> Bool
isInAnyTeam c=
 c.team /= NoTeam

isInTeam: Team -> Character -> Bool
isInTeam team c=
    c.team == team

isOppositeTeam: Character -> Character -> Bool
isOppositeTeam c1 c2 =not ( (c2.team == NoTeam) || (c1.team == NoTeam) || (c1.team == c2.team) )

charactersInTeam : Team -> List Character -> List Character
charactersInTeam team characters =
    List.filter (isInTeam team) characters

generateCharacterWithTeam : (String, Team) -> Character
generateCharacterWithTeam (type_,team)=
 generateCharacter type_ |> addTeam team

generateCharacter : String -> Character
generateCharacter type_ =
    let
        base  = {
            name = "Bad"
            ,type_="Bad"
            ,id=0
            ,team=NoTeam
            ,baseHP = 0, currentHP=0, weaponDmg = 0, weaponSkill = 0
            , toughness=0
            ,initiative =0
            ,acted=False
         }
    in
    
        (case type_ of
            "Goblin" ->
                { base|  baseHP = 11, weaponDmg = 7, weaponSkill = 25, initiative=20, toughness=40 }

            "Wolf" ->
                {base|   baseHP = 10, weaponDmg = 6, weaponSkill = 35 ,initiative=35, toughness=40 }
            "Human" ->
                {base|   baseHP = 12, weaponDmg = 7, weaponSkill = 30 ,initiative=30, toughness=30 }
            "Dwarf" ->
                {base|   baseHP = 16, weaponDmg = 7, weaponSkill = 40 ,initiative=30, toughness=40 }
            "Ogre" ->
                {base|   baseHP = 28, weaponDmg = 8, weaponSkill = 30 ,initiative=10, toughness=45 }
            _ ->
                 base 
        ) |> addType type_
          |> addName type_
          |> resetHP


badCharacter : Character
badCharacter =
    generateCharacter "BAD"

addType : String -> Character -> Character
addType type_ c =
 {c|type_=type_}
addName : String -> Character -> Character
addName name c=
 {c|name=name}

resetHP: Character -> Character
resetHP c=
 {c|currentHP=c.baseHP}

addTeam: Team -> Character -> Character
addTeam team c=
 {c|team=team}

countTeamCharactersActive characters=
 let
        aliveList =
            listAliveCharacters characters
        homeCount =List.length <| List.filter (\c->c.team==Home) aliveList
        awayCount = List.length <|List.filter (\c->c.team==Away) aliveList

  in 
    (homeCount,awayCount)
 
dealDamage : Character -> Character -> Int -> ( Character, String )
dealDamage attacker defender success=
    let
        dealt=max (attacker.weaponDmg+success-(Debug.log "T:" (defender.toughness//10))) 0
        remainingHP= max (defender.currentHP - dealt) 0
        
    in
    
    ( { defender | currentHP = remainingHP }, attacker.name ++ " hit " ++ defender.name ++ " for " ++ String.fromInt dealt ++ (if remainingHP<1 then " killing them! " else ""))

opposedSkillTest : Int -> Int -> Int -> Int -> Int
opposedSkillTest attRoll defRoll attSkill defSkill=
 (attSkill//10 - attRoll//10) - (defSkill//10-defRoll//10)

attackCharacter : (Int,Int) -> Character -> Character -> ( Character, String )
attackCharacter (attRoll,defRoll) attacker defender =
 let
     success=opposedSkillTest (Debug.log "Att roll" attRoll) (Debug.log "Def roll" defRoll) attacker.weaponSkill defender.weaponSkill
 in
 
    if (Debug.log "succ: " success) >0 then
        dealDamage attacker defender success

    else
        ( defender, attacker.name ++ " missed " ++ defender.name )

getCharacter : Int -> Array Character ->  Character
getCharacter id characters=
    Array.get id characters |> Maybe.withDefault badCharacter

updateCharacter : Character ->  Array Character -> Array Character
updateCharacter character characters=
    Array.map (\c -> if c.id==character.id then character else c) characters

listAliveCharacters : Array Character -> List Character
listAliveCharacters arr =
    Array.filter isAlive arr
    |> Array.filter isInAnyTeam
    |> Array.toList


isAlive : Character -> Bool
isAlive character =
    character.currentHP > 0


listOfOpponents : Array Character -> Int -> List Character
listOfOpponents characters characterId =
    Array.filter isAlive characters
        |> Array.filter (\c -> not (c.id == characterId))
        |> Array.filter (isOppositeTeam (Array.get characterId characters |> Maybe.withDefault badCharacter))
        |> Array.toList

isAttackable attacker character=
 (isOppositeTeam attacker character) && (isAlive character)


isInBattle character=
 (isAlive character) && (isInAnyTeam character)
listStillToAct : List Character -> List Character
listStillToAct inBattleCharacters= 
 List.filter (\c-> not c.acted) inBattleCharacters 
 |> List.sortBy .initiative 
 |> List.reverse

listActed : List Character -> List Character
listActed inBattleCharacters= 
 List.filter .acted inBattleCharacters 
 |> List.sortBy .initiative 
 |> List.reverse