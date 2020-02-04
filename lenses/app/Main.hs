{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE  FlexibleContexts #-}
{-# LANGUAGE  RankNTypes #-}
{-# LANGUAGE  ScopedTypeVariables #-}
{-# LANGUAGE  TypeApplications #-}
{-# LANGUAGE  TypeFamilies #-}

module Main where

import Lib

test = Pet { _petName = "test", _petType = "test"}

main :: IO ()
main = someFunc
