module Optics where

import {- lens       -} Control.Lens
import {- containers -} Data.Set qualified as Set

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

nats = Set.fromList [0, 1, 2]

-- $> nats ^.. folded
-- [0, 1, 2]

{-

^.  Get focused element (exactly 1 focus)
        _1 (first element of tuple)

^.. Get iterator for all focused elements (at least 0 foci)
        folded (all the elements of a Foldable container)
        both (both elements of a Pair a a / Either a a)
        each (all elements of a tuple, list, Text, ByteString)

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