{-# LANGUAGE OverloadedStrings #-}
import Test.HUnit hiding (Test)
import Test.Hspec.Monadic
import Test.Hspec.HUnit ()
import Data.Conduit
import qualified Data.Conduit.List as CL
import Text.XML.Expat.Conduit
import Data.XML.Types
import Data.Default
import Data.ByteString.Char8 ()
import qualified Control.Exception as E
import Control.Applicative

parseExpat s =
    runResourceT $
    yield s $= 
    parseBytes def $$ 
    CL.consume

parseXml s =
    runResourceT $
    yield s $=
    parseBytes def $=
    expatToXml $$
    CL.consume

main :: IO ()
main = 
    hspec $ do
      describe "parseBytes" $ expatParsing
      describe "parseBytes $$ expatToXml" $ xmlParsing
        
expatParsing = do
  
  it "parses at all" $ 
     do _ <- parseExpat "<d/>"
        return () :: IO ()
        
  it "parses a simple document" $ 
     do d <- parseExpat "<r/>"
        d @?= [ StartElement "r" []
              , EndElement "r"
              ]
          
  it "parses one attribute" $ 
     do d <- parseExpat "<r foo='bar'/>"
        d @?= [ StartElement "r" [("foo", "bar")]
              , EndElement "r"
              ]
          
  it "parses two attributes" $ 
     do d <- parseExpat "<r foo='bar' baz='quux'/>"
        d @?= [ StartElement "r" [ ("foo", "bar")
                                 , ("baz", "quux")
                                 ]
              , EndElement "r"
              ]

  it "parses character data" $
     do d <- parseExpat "<r>foobar</r>"
        d @?= [ StartElement "r" []
              , CharacterData "foobar"
              , EndElement "r"
              ]
          
  it "throws upon incomplete document" $
     do e <- E.catch (Right <$> parseExpat "<r>foo") $
             return . Left
        e @?= Left ExpatError
          
xmlParsing = do
    
  it "parses at all" $ 
     do _ <- parseXml "<d/>"
        return () :: IO ()
        
  it "parses a simple document" $ 
     do d <- parseXml "<r/>"
        d @?= [ EventBeginDocument 
              , EventBeginElement "r" []
              , EventEndElement "r"
              , EventEndDocument
              ]
          
  it "parses one attribute" $ 
     do d <- parseXml "<r foo='bar'/>"
        d @?= [ EventBeginDocument 
              , EventBeginElement "r" [("foo", [ContentText "bar"])]
              , EventEndElement "r"
              , EventEndDocument
              ]
          
  it "parses two attributes" $ 
     do d <- parseXml "<r foo='bar' baz='quux'/>"
        d @?= [ EventBeginDocument 
              , EventBeginElement "r" [ ("foo", [ContentText "bar"])
                                      , ("baz", [ContentText "quux"])
                                      ]
              , EventEndElement "r"
              , EventEndDocument
              ]

  it "parses character data" $
     do d <- parseXml "<r>foobar</r>"
        d @?= [ EventBeginDocument 
              , EventBeginElement "r" []
              , EventContent (ContentText "foobar")
              , EventEndElement "r"
              , EventEndDocument
              ]
          
  it "parses xmlns" $
     do d <- parseXml "<r xmlns='foo'><c/></r>"
        d @?= [ EventBeginDocument 
              , EventBeginElement
                Name { nameLocalName = "r"
                     , nameNamespace = Just "foo"
                     , namePrefix = Nothing
                     } []
              , EventBeginElement
                Name { nameLocalName = "c"
                     , nameNamespace = Just "foo"
                     , namePrefix = Nothing
                     } []
              , EventEndElement
                Name { nameLocalName = "c"
                     , nameNamespace = Just "foo"
                     , namePrefix = Nothing
                     }
              , EventEndElement
                Name { nameLocalName = "r"
                     , nameNamespace = Just "foo"
                     , namePrefix = Nothing
                     }
              , EventEndDocument
              ]
  
  it "parses prefixed xmlns" $
     do parseExpat "<p:r xmlns:p='bar'/>" >>= print
        d <- parseXml "<p:r xmlns:p='bar'/>"
        d @?= [ EventBeginDocument 
              , EventBeginElement
                Name { nameLocalName = "r"
                     , nameNamespace = Just "bar"
                     , namePrefix = Just "p"
                     } []
              , EventEndElement
                Name { nameLocalName = "r"
                     , nameNamespace = Just "bar"
                     , namePrefix = Just "p"
                     }
              , EventEndDocument
              ]
  
  it "parses nested prefixed xmlns" $
     do d <- parseXml "<p:r xmlns:p='foo'><p:c xmlns:p='bar'/><p:c/></p:r>"
        d @?= [ EventBeginDocument 
              , EventBeginElement
                Name { nameLocalName = "r"
                     , nameNamespace = Just "foo"
                     , namePrefix = Just "p"
                     } []
              , EventBeginElement
                Name { nameLocalName = "c"
                     , nameNamespace = Just "bar"
                     , namePrefix = Just "p"
                     } []
              , EventEndElement
                Name { nameLocalName = "c"
                     , nameNamespace = Just "bar"
                     , namePrefix = Just "p"
                     }
              , EventBeginElement
                Name { nameLocalName = "c"
                     , nameNamespace = Just "foo"
                     , namePrefix = Just "p"
                     } []
              , EventEndElement
                Name { nameLocalName = "c"
                     , nameNamespace = Just "foo"
                     , namePrefix = Just "p"
                     }
              , EventEndElement
                Name { nameLocalName = "r"
                     , nameNamespace = Just "foo"
                     , namePrefix = Just "p"
                     }
              , EventEndDocument
              ]

  it "parses CDATA" $
     do d <- parseXml "<r><![CDATA[frobfrobfrob]]></r>"
        d @?= [ EventBeginDocument
              , EventBeginElement "r" []
              , EventCDATA "frobfrobfrob"
              , EventEndElement "r"
              , EventEndDocument
              ]
          
  it "parses DOCTYPE" $
     do d <- parseXml "<!DOCTYPE html><html/>"
        d @?= [ EventBeginDocument
              , EventBeginDoctype "html" Nothing
              , EventEndDoctype
              , EventBeginElement "html" []
              , EventEndElement "html"
              , EventEndDocument
              ]
          