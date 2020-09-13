{-# LANGUAGE FlexibleInstances #-}
module Main where

import Control.Monad.Supply

--instance MonadFail (Either String) where
--  fail s = Left s

test :: SupplyT Int Maybe Int
test = do
    supply 
    supply 
    supply

xxx :: Maybe Int
xxx = evalSupplyT test [1,2,3]

main :: IO ()
main = do
  putStrLn "hello world"
