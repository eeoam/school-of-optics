module Lens where

import {- lens -} Control.Lens

-- tuple : { _1 : Bool, _2 : Int } = Bool x Int
tuple = (True, 0)

-- $> tuple ^. _1 
{- == True -}

-- $> tuple & _1 .~ False 
{- == (False,0) -}

-- $> tuple & _2 %~ (+1)
{- == (True, 1) -}

-- $> tuple & _2 +~ 1
{- == (True, 1) -}

-- $> tuple & _1 &&~ False
{- == (False, 0) -}

-- $> tuple & _1 <&&~ False
{- == (False, (False, 0)) -}

-- $> tuple & _1 <<&&~ False
{- == (True, (False, 0)) -}


{-
ghciwatch --command "stack repl" --watch src --enable-eval

-- stack repl
-- stack haddock

-- Hoogle:
-- stack build --fast --haddock-deps
-- stack hoogle -- generate --local (rerun when add/change dependency)
-- stack hoogle -- server --local --port=8080
-}