{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
module Lib
    ( someFunc
    ) where

import Control.Eff.Coroutine( Yield(..)
                            , yield
                            , runC
                            , Y(..))
import Control.Eff.State.Strict
import Control.Eff

func :: Y r Integer Float -> Integer
func (Y a b) = a
func (Done)  = 1

funcB :: Y r Integer Float -> Eff r (Y r Integer Float)
funcB (Y a b) = b 3.0
funcB (Done) = error "String"

funct :: Eff '[] (Y '[] Integer Integer)
funct = runC (do yield 4; let x = 1333 in yield x; return 3)

runMult (Y a b) = runMult (run $ b 3)
runMult (Done)  = 42
		       

someFunc :: IO ()
someFunc = putStrLn "someFunc"
