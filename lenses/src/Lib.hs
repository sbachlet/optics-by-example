{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE  FlexibleContexts #-}
{-# LANGUAGE  RankNTypes #-}
{-# LANGUAGE  ScopedTypeVariables #-}
{-# LANGUAGE  TypeApplications #-}
{-# LANGUAGE  TypeFamilies #-}

module Lib
    ( someFunc
    , Pet(..)
    , petName
    , petType
    , Err(..)
    , msg
    ) where

import Control.Lens
import Control.Applicative ()
import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Ship = Ship
  {
    _name    :: String
  , _numCrew :: Int
  } deriving (Show)

makeLenses ''Ship

purplePearl :: Ship
purplePearl = Ship
  {
    _name    = "Purple Pearl"
  , _numCrew = 38
  }

data Pet = Pet
  {
    _petName :: String
  , _petType :: String
  } deriving (Show, Eq)

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
    set (True, _, y) z = (True, z, y)
    set (False, x, _) z = (False, x, z)

data Err =
    ReallyBadError { _msg :: String }
  | ExitCode       { _code :: Int }

msg :: Lens' Err String
msg = lens getMsg setMsg 
    where
        getMsg (ReallyBadError message) = message 
        getMsg (ExitCode _) = ""
        setMsg (ReallyBadError _) newMessage = ReallyBadError newMessage
        setMsg (ExitCode n) newMessage = ExitCode n