module Utils exposing (..)

addOverflow a delta max=
 Basics.remainderBy max (a+delta) 
 
incOverflow a max=
 addOverflow a 1 max