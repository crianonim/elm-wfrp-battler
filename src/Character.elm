module Character exposing (..)

import Array exposing (Array)


type alias Character =
    { name : String
    , type_ : String
    , baseHP : Int
    , currentHP : Int
    , weaponDmg : Int
    , weaponSkill : Int
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

isInTeam : Character -> Bool
isInTeam c=
 not (c.team == NoTeam)

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
         }
    in
    
        (case type_ of
            "Goblin" ->
                { base|  baseHP = 4, weaponDmg = 3, weaponSkill = 30 }

            "Wolf" ->
                {base|   baseHP = 22, weaponDmg = 5, weaponSkill = 55 }

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
 
dealDamage : Character -> Character -> ( Character, String )
dealDamage attacker defender =
    ( { defender | currentHP = defender.currentHP - attacker.weaponDmg }, attacker.name ++ " hit " ++ defender.name ++ " for " ++ String.fromInt attacker.weaponDmg )


attackCharacter : Int -> Character -> Character -> ( Character, String )
attackCharacter roll attacker defender =
    if roll < attacker.weaponSkill then
        dealDamage attacker defender

    else
        ( defender, attacker.name ++ " missed " ++ defender.name )


listAliveCharacters : Array Character -> List Character
listAliveCharacters arr =
    Array.filter isAlive arr
    |> Array.filter isInTeam 
    |> Array.toList


isAlive : Character -> Bool
isAlive character =
    character.currentHP > 0

isOppositeTeam: Character -> Character -> Bool
isOppositeTeam c1 c2 =not ( (c2.team == NoTeam) || (c1.team == NoTeam) || (c1.team == c2.team) )
listOfOpponents : Array Character -> Int -> List Character
listOfOpponents characters characterId =
    Array.filter isAlive characters
        |> Array.filter (\c -> not (c.id == characterId))
        |> Array.filter (isOppositeTeam (Array.get characterId characters |> Maybe.withDefault badCharacter))
        |> Array.toList
