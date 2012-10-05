{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Conduit
import Data.Conduit.List (sinkNull)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LBC
import qualified Text.XML.Stream.Parse as XML
import qualified Text.XML.Expat.Conduit as Expat
import Data.Default (def)
import Control.Monad
import Criterion.Main (defaultMain, bench)


data Lib = XmlConduit | LibExpat
         deriving (Show, Eq, Enum, Bounded)
data Strictness = Lazy | Strict
                deriving (Show, Eq, Enum, Bounded)

buf size = head $
           filter ((>= size) . LBC.length) $
           bufs ""
bufs s = let s' = LBC.concat [ "<r>\n"
                             , s
                             , "</r>\n"
                             ]
         in s' : bufs s'

main = do
  defaultMain $ do
    size <- [1, 2, 4] --, 16, 64] --, 512, 1024]
    strictness <- [minBound..maxBound]
    lib <- [minBound..maxBound]
    let s = buf $ size * 1024
        source Lazy lb = mapM_ yield $ LBC.toChunks lb
        source Strict lb = let b = BC.concat $ LBC.toChunks lb
                           in b `seq`
                              yield b
        libConduit XmlConduit = XML.parseBytes def
        libConduit LibExpat = Expat.parseBytes def =$= Expat.expatToXml
        title = show lib ++ " " ++ 
                show strictness ++ " " ++ 
                show size
        f :: IO ()
        f = 
          runResourceT $
          source strictness s $=
          libConduit lib $$ 
          sinkNull
    return $ bench title f
  