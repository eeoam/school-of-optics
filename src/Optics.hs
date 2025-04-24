{-# LANGUAGE OverloadedStrings #-}

module Optics where

import {- lens       -} Control.Lens
import {- containers -} Data.Set qualified as Set
import {- containers -} Data.Map qualified as Map
import {- text       -} Data.Text ( Text )
import {- text       -} Data.Text qualified as Text
import {- bytestring -} Data.ByteString (ByteString)
import {- bytestring -} Data.ByteString qualified as Bytes
import {- text       -} Data.Text.Encoding ( encodeUtf8 )

import {- base       -} Data.Ord ( comparing )
import {- base       -} Data.Monoid ( Sum(..) )

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

-- $> (1, 2, 3) & traverseOf_ each print
-- 1
-- 2
-- 3

average (Sum total, Sum count) = fromIntegral total / fromIntegral count

-- $> average $ (1, 2, 3) & foldOf (each . to (\x -> (Sum x, Sum 1)))
-- 2

-- $> average $ (1, 2, 3) & foldMapOf each (\x -> (Sum x, Sum 1))
-- 2

-- $> ('a', 'b', 'b', 'c', 'c', 'c') & foldMapByOf each (Map.unionWith (+)) Map.empty (\c -> Map.singleton c 1)
-- {'a'::1 'b'::2 'c'::3}

-- $> ("Agda", "Haskell") ^.. each . taking 2 folded
-- "AgHa"

-- $> ("Agda", "Haskell") ^.. taking 2 (each . folded)
-- "Ag"

-- $> ("Agda", "Haskell") ^.. each . dropping 2 folded
-- "daskell"

-- $> ("Agda", "Haskell") ^.. dropping 2 (each . folded)
-- "daHaskell"


-- $> ("Agda", "Haskell") ^.. each . backwards folded
-- = "adgAlleksaH"

-- $> ("Agda", "Haskell") ^.. backwards (each . folded)
-- = "lleksaHadgA"

-- $> [1..20] ^.. takingWhile (<10) folded
-- [1,2,3,4,5,6,7,8,9]

-- $> [1..20] ^.. droppingWhile (<10) folded
-- [10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]

-- $> (1,2,3,4) ^.. each . filtered even
-- [2,4]

-- $> 'a' ^? only 'a'
-- Just ()

-- $> 'a' ^? only 'A'
-- Nothing

-- $> (1,2,3,2) ^.. each . filteredBy (only 2)
-- [2,2]

--148
-- $> (False, False) & both .~ True
-- (True, True)

-- $> import Data.Char (toUpper)
-- $> Text.pack "Bool" & each %~ toUpper
-- "BOOL"

-- $> ["AB", "ABC"] & each . filtered ((>2) . length) %~ reverse
-- ["AB", "CBA"]

-- $> [1,2,3] & traversed *~ 2

-- $> "ludwig von mises" & worded %~ \(x:xs) -> toUpper x : xs
-- "Ludwig Von Mises"

-- $> "agda\nhaskell" & lined %~ \(x:xs) -> toUpper x : xs
-- "Agda\nHaskell"

ltuple, rtuple :: Either (Int,Int) (Int,Int,Int)
ltuple = Left (1,2)
rtuple = Right (3,4,5)  

-- $> ltuple & beside both each %~ negate
-- Left (-1, -2)

-- $> rtuple & beside both each %~ negate
-- Right (-3, -4)

-- $> [0,1,2,3] & element 3 *~ 2
-- [0,1,2,6]

-- $> ((0,1),(2,3)) & elementOf (each . each) 3 *~ 2
-- ((0,1),(2,6))

--158

-- $> import Text.Read (readMaybe)
-- $> ("0", "1") & traverseOf each readMaybe :: Maybe (Int, Int)
-- Just (0,1)

-- infix traverseOf %%~
-- $> ("0", "1" ) & each %%~ readMaybe :: Maybe (Int, Int)

partsOfeg = [('a', 1), ('b', 2), ('c', 3), ('d', 2), ('e',2)] 
          & partsOf (traversed . _2)
          %~ \xs -> (/ sum xs) <$> xs
-- $> partsOfeg


-- $> Text.pack "fuse" & ix 0 .~ 'm'
-- muse
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