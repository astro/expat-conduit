{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Conduit
import qualified Data.ByteString.Char8 as BC
import Text.XML.Expat.Conduit
import Data.Default (def)


main = do
  let buf =
        foldl (\buf' _ ->
                BC.concat [ "<r>\n"
                          , buf'
                          , "</r>\n"
                          ]
              ) "" [1..20000]
  events <- runResourceT $
            ({-# SCC "yield" #-} yield buf) $=
            parseBytes def =$= 
            ({-# SCC "expatToXml" #-} expatToXml) $$
            countSink
  putStrLn $ "Consumed " ++ show events ++ " events"
  
  
countSink' n =
  n `seq`
  await >>=
  maybe (return n) (const $
                    {-liftIO (putStrLn $ "Got " ++ show n)
                    >>-}
                    countSink' (n + 1)
                   )

countSink =
  countSink' 0
