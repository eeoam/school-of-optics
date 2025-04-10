{-# LANGUAGE OverloadedStrings #-}

module Optics where

import {- lens       -} Control.Lens
import {- containers -} Data.Set qualified as Set
import {- text       -} Data.Text ( Text )
import {- text       -} Data.Text qualified as Text
import {- bytestring -} Data.ByteString (ByteString)
import {- bytestring -} Data.ByteString qualified as Bytes
import {- text       -} Data.Text.Encoding ( encodeUtf8 )

import {- base       -} Data.Ord ( comparing )

{-----Lenses-----}

-- pair : { _1 : Bool, _2 : Int } = Bool x Int
pair = (True, 0)

-- $> pair ^. _1 
{- == True -}

-- $> pair & _1 .~ False 
{- == (False,0) -}

-- $> pair & _2 %~ (+1)
{- == (True, 1) -}

-- $> pair & _2 +~ 1
{- == (True, 1) -}

-- $> pair & _1 &&~ False
{- == (False, 0) -}

-- $> pair & _1 <&&~ False
{- == (False, (False, 0)) -}

-- $> pair & _1 <<&&~ False
{- == (True, (False, 0)) -}


{-----Folds-----} 

-- $> Set.fromList [0, 1, 2] ^.. folded
-- [0, 1, 2]

-- $> (True, False) ^.. both
-- [True, False]

-- $> Left True ^.. both
-- [ True ]

-- $> Right False ^.. both
-- [ False ]

-- $> (1,2,3,4,5) ^.. each
-- [1,2,3,4,5]

-- $> [1,2,3,4,5] ^.. each
-- [1,2,3,4,5] 

-- $> Text.pack "hello"  ^.. each
-- ['h', 'e', 'l', 'l', 'o']

-- $> encodeUtf8 (Text.pack "hello") ^.. each
-- [104, 101, 108, 108, 111]

data A = A Int (Int, Int)
-- $> A 0 (1,2) ^.. folding (\(A x (y,z)) -> x : [y, z])
-- [0, 1, 2]

data Name = Name { firstName, surname :: Text }
name = Name (Text.pack "Carl") (Text.pack "Menger")


-- $> name ^.. to firstName
-- [ "Carl" ]

-- $> name ^. to firstName
-- "Carl"

-- $> [1, 2, 3, 4] & elemOf folded 3
-- True

-- $> ('a', 'b') & elemOf both 'a'
-- True

-- $> (1, 2, 3, 4) & anyOf each even
-- True

-- $> (1, 2, 3, 4) & allOf each odd
-- False

-- $> (1, 4, 3, 2) & findOf each even
-- Just 4

-- $> ('a', 'b', 'c', 'd') & has each
-- True

-- $> [] & hasn't folded
-- True

-- $> ('a', 'b', 'c', 'd') & lengthOf each
-- 4

-- $> ('a', 'b') & firstOf each
-- Just 'a'

-- $> ('a', 'b') & preview each
-- Just 'a'

-- $> ('a', 'b') ^? each
-- Just 'a'

-- $> ('a', 'b') & lastOf each
-- Just 'b'

-- $> (1, 2, 3, 4) & minimumOf each
-- Just 1

-- $> (1, 2, 3, 4) & maximumOf each
-- Just 4

-- $> ('a', 'b', 'c') & maximumByOf each (comparing id)
-- Just 'c'

-- $> ('a', 'b', 'c') & minimumByOf each (comparing id)
-- Just 'a'
{-

ghciwatch --command "stack repl" --watch src --watch package.yaml --enable-eval

^.  Get focused element (exactly 1 focus)
        _1 (first element of tuple)

^.. Get iterator for all focused elements (at least 0 foci) (type Iterator a = [ a ])
        folded (all the elements of a Foldable container)
        both (both elements of a Pair a a / Either a a)
        each (all elements of a tuple, list, Text, ByteString)

        folding
        to 
        elemOf

-}



{-
ghciwatch --command "stack repl" --watch src --enable-eval

-- stack repl
-- stack haddock

-- Hoogle:
-- stack build --fast --haddock-deps
-- stack hoogle -- generate --local (rerun when add/change dependency)
-- stack hoogle -- server --local --port=8080
-}