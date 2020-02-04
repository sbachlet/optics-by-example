{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

import Lib

import Control.Lens
import Test.Hspec
import Text.Printf (printf)

setGetLawfull :: (Show a, Eq a) => String -> Lens' s a -> a -> s -> Spec
setGetLawfull lensName lens value structure = 
  it (lensName <> " set x on y then view y should return x") $ do
    (view lens (set lens value structure)) `shouldBe` value 

getSetLawfull :: (Show s, Eq s) => String -> Lens' s a -> s -> Spec
getSetLawfull lensName lens structure =
  it (lensName <> " set x on y where x is viewed from y is a no op") $ do
    (set lens (view lens structure) structure) `shouldBe` structure  

setSetLawfull :: (Show s, Eq s) => String -> Lens' s a -> a -> a -> s -> Spec
setSetLawfull lensName lens value0 value1 structure = 
  it (lensName <> " setting x1 on y after setting x0 should be equal to setting x1 on y") $ do
    (set lens value0 (set lens value1 structure)) `shouldBe` (set lens value0 structure)

setGetUnlawfull :: (Show a, Eq a) => String -> Lens' s a -> a -> s -> Spec
setGetUnlawfull lensName lens value structure = 
  it (lensName <> " set x on y then view y should not return x") $ do
    (view lens (set lens value structure)) `shouldNotBe` value 

testPet :: Pet
testPet = 
  Pet { _petName = "test pet name"
      , _petType = "test pet type"
      }

newMessage = "False alarm!"
testErr0 = ReallyBadError "BAD BAD BAD"
testErr1 = ExitCode 1 

main :: IO ()
main = hspec $ do
  describe "Pet Lens Laws:" $ do 
    setGetLawfull "petName" petName "test name" testPet
    setGetLawfull "petType" petType "test type" testPet
    
    getSetLawfull "petName" petName testPet
    getSetLawfull "petType" petType testPet
    
    setSetLawfull "petName" petName "test name" "test name 2" testPet
    setSetLawfull "petType" petType "test type" "test type 2" testPet 
  describe "Case study Err:" $ do 
    setGetLawfull "msg" msg newMessage testErr0 
    setGetUnlawfull "msg" msg newMessage testErr1

