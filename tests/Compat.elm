module Compat exposing (..)

import Dict

values = 
  Dict.fromList([("first", True),("second", False),("third", True),("fourth", False),("fifth", True)])

cases = 
    [ ("first AND second OR third", True)
    , ("first AND second AND third", False)
    , ("first XOR second AND third OR fourth", True)
    , ("first OR second AND third", True)
    , ("first AND second XOR third OR fourth", True)
    , ("first OR second AND third XOR fourth", True)
    , ("first OR second XOR third AND fourth", True)
    , ("first OR NOT second XOR third AND NOT fourth", True)
    , ("(first OR second) AND third", True)
    , ("first XOR (second AND third)", True)
    , ("first XOR (second AND third) OR fourth", True)
    , ("NOT ((first OR second) AND NOT third) XOR (NOT fourth AND fifth)", False)
    , ("NOT (first AND second AND NOT third) XOR (NOT fourth XOR fifth)", True)
    ]