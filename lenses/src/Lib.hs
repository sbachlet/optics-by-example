{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE RecordWildCards     #-}

module Lib
  ( someFunc
  , Pet(..)
  , petName
  , petType
  , Err(..)
  , msg
  , msg'
  , Ship(..)
  , name
  , numCrew
  , AllThree(..)
  , switch
  , Builder(..)
  , context
  ) where

import           Control.Applicative ()
import           Control.Lens
import           Data.Char
import qualified Data.Map            as M
import qualified Data.Set            as S
import qualified Data.Text           as T

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Ship = Ship
    { _name    :: String
    , _numCrew :: Int
    }
    deriving (Show, Eq)

-- makeLenses ''Ship

purplePearl :: Ship
purplePearl = Ship
  {
    _name    = "Purple Pearl"
  , _numCrew = 38
  }

data Pet = Pet
    { _petName :: String
    , _petType :: String
    }
    deriving (Show, Eq)

makeLenses ''Pet

second :: Lens' (a, b, c) b
second = lens get set
  where
    get :: (a, b, c) -> b
    get (_, b, _) = b

    set :: (a, b, c) -> b -> (a, b, c)
    set (x, _, z) b = (x, b, z)

conditional :: Lens' (Bool, a, a) a
conditional = lens get set
  where
    get :: (Bool, a, a) -> a
    get (True, x, _)  = x
    get (False, _, x) = x

    set :: (Bool, a, a) -> a -> (Bool, a, a)
    set (True, _, y) z  = (True, z, y)
    set (False, x, _) z = (False, x, z)

data Err = ReallyBadError
    { _msg :: String
    }
    | ExitCode
    { _code :: Int
    }
    deriving (Eq, Show)

msg :: Lens' Err String
msg = lens getMsg setMsg
  where
    getMsg (ReallyBadError message) = message
    getMsg (ExitCode _)             = ""
    setMsg (ReallyBadError _) newMessage = ReallyBadError newMessage
    setMsg (ExitCode n) newMessage       = ExitCode n

-- Exercises - Laws
-- 1.
name :: Lens' Ship String
name = lens getter setter
  where
    getter (Ship name _) = name
    setter _ name = Ship name 10

numCrew :: Lens' Ship Int
numCrew = lens getter setter
  where
    getter (Ship _ crew) = crew
    setter (Ship name oldNumCrew) numCrew =
      if oldNumCrew `mod` 2 == 0
      then Ship name (oldNumCrew `div` 2)
      else Ship name numCrew

-- 3.
msg' :: Lens' Err String
msg' = lens getMsg setMsg
    where
      getMsg (ReallyBadError message) = message
      getMsg (ExitCode _) = "" 
      setMsg _ newMsg = ReallyBadError newMsg
-- 4
data CouldBe =
    Definitley String
  | NoWay
maybeLens :: Lens' CouldBe String 
maybeLens = lens get set
  where
    get (Definitley x) = x
    get NoWay = ""
    set _ x = Definitley x

-- 5
data AllThree = AllThree
  { _one :: String
  , _two :: String
  } deriving (Eq, Show)

switch :: Lens' AllThree String
switch = lens get set
  where
    get AllThree{..} = _two
    set AllThree{..} _ = AllThree _two _one

-- 6
data Builder =
  Builder { _context :: [String]
          , _build   :: [String] -> String
          }

context :: Lens' Builder String
context = lens get set
  where
    get (Builder c  b) = b c
    set (Builder c b) newContext = 
      Builder [newContext] $ \c' ->
        if c' == c
        then newContext
        else b c'
-- End Exercises - Laws
