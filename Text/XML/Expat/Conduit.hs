{-# LANGUAGE OverloadedStrings #-}
module Text.XML.Expat.Conduit where

import Data.Conduit
import Control.Monad.Trans
import qualified Data.ByteString as B
import Data.XML.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Data.Default
import Data.IORef
import qualified Data.Sequence as Seq
import Data.Sequence ((|>))
import Control.Applicative
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Foldable (toList)
import System.IO
import qualified Data.Map as Map

import Text.XML.Expat.Conduit.Internal


data ExpatEvent = StartElement Text [(Text, Text)]
                | EndElement Text
                | CharacterData Text
                | ProcessingInstruction Text Text
                | Comment Text
                | StartCdataSection
                | EndCdataSection
                | StartDoctypeDecl Text Text Text Bool
                | EndDoctypeDecl
                -- EntityDecl
                | NotationDecl Text Text Text Text
                  deriving (Eq, Show)


data ParseSettings = ParseSettings

instance Default ParseSettings where
    def = ParseSettings


parseBytes :: MonadResource m =>
              ParseSettings ->
              Conduit B.ByteString m ExpatEvent
parseBytes _ =
    do
      -- Output buffer
      events <- liftIO $ newIORef Seq.empty
      let addEvent e =
              readIORef events >>=
              writeIORef events . (|> e)
          flushEvents =
              toList <$> readIORef events <*
              writeIORef events Seq.empty
      
      -- SAX Handlers
      startElementHandler <-
          liftIO $
          mkStartElementHandler $ \_ name attrs ->
              do name' <- cstringToText name
                 attrs' <- attrsToTexts attrs
                 addEvent $ StartElement name' attrs'
      endElementHandler <-
          liftIO $
          mkEndElementHandler $ \_ name ->
              cstringToText name >>=
              addEvent . EndElement
      characterDataHandler <-
          liftIO $
          mkCharacterDataHandler $ \_ buf len ->
              decodeUtf8 <$> B.packCStringLen (buf, fromIntegral len) >>=
              addEvent . CharacterData
      processingInstructionHandler <-
          liftIO $
          mkProcessingInstructionHandler $ \_ target data_ ->
              ProcessingInstruction <$>
              cstringToText target <*>
              cstringToText data_ >>=
              addEvent
      commentHandler <-
          liftIO $
          mkCommentHandler $ \_ s ->
              cstringToText s >>=
              addEvent . Comment
      startCdataSectionHandler <-
          liftIO $
          mkStartCdataSectionHandler $ \_ ->
              addEvent StartCdataSection
      endCdataSectionHandler <-
          liftIO $
          mkEndCdataSectionHandler $ \_ ->
              addEvent EndCdataSection
      startDoctypeDeclHandler <-
          liftIO $
          mkStartDoctypeDeclHandler $ \_ doctypeName sysid pubid hasInternalSubset ->
              StartDoctypeDecl <$>
              cstringToText doctypeName <*>
              cstringToText sysid <*>
              cstringToText pubid <*>
              (pure $ hasInternalSubset /= 0) >>=
              addEvent
      endDoctypeDeclHandler <-
          liftIO $
          mkEndDoctypeDeclHandler $ \_ ->
              addEvent EndDoctypeDecl
      entityDeclHandler <-
          liftIO $
          mkEntityDeclHandler $ \_ entityName isParameterEntity value valueLen base systemId publicId notationName ->
              --addEvent EntityDecl
              return ()
      notationDeclHandler <-
          liftIO $
          mkNotationDeclHandler $ \_ notationName base systemId publicId ->
              (NotationDecl <$>
              cstringToText notationName <*>
              cstringToText base <*>
              cstringToText systemId <*>
              cstringToText publicId) >>=
              addEvent
          
      -- conduitIO parameters
      let alloc = 
              do encoding <- newCString "UTF-8"
                 parser <- xmlParserCreate encoding
                 xmlSetStartElementHandler parser startElementHandler
                 xmlSetEndElementHandler parser endElementHandler
                 xmlSetCharacterDataHandler parser characterDataHandler
                 xmlSetProcessingInstructionHandler parser processingInstructionHandler
                 xmlSetCommentHandler parser commentHandler
                 xmlSetStartCdataSectionHandler parser startCdataSectionHandler
                 xmlSetEndCdataSectionHandler parser endCdataSectionHandler
                 xmlSetStartDoctypeDeclHandler parser startDoctypeDeclHandler
                 xmlSetEndDoctypeDeclHandler parser endDoctypeDeclHandler
                 xmlSetEntityDeclHandler parser entityDeclHandler
                 xmlSetNotationDeclHandler parser notationDeclHandler
                 return parser
          cleanup parser =
              do xmlParserFree parser
                 freeHaskellFunPtr startElementHandler
                 freeHaskellFunPtr endElementHandler
                 freeHaskellFunPtr characterDataHandler
                 freeHaskellFunPtr processingInstructionHandler
                 freeHaskellFunPtr commentHandler
                 freeHaskellFunPtr startCdataSectionHandler
                 freeHaskellFunPtr endCdataSectionHandler
                 freeHaskellFunPtr startDoctypeDeclHandler
                 freeHaskellFunPtr endDoctypeDeclHandler
                 freeHaskellFunPtr entityDeclHandler
                 freeHaskellFunPtr notationDeclHandler
          push parser buf =
              liftIO $
              B.useAsCStringLen buf $ \(str, len) ->
                  do let len' = fromIntegral len
                     _status <- xmlParse parser str len' 0
                     IOProducing <$> flushEvents
          close _parser =
              -- TODO: finalize
              return []
              
      conduitIO alloc cleanup push close

cstringToText :: CString -> IO Text
cstringToText s = decodeUtf8 <$> B.packCString s

cstringToMaybeText :: CString -> IO (Maybe Text)
cstringToMaybeText s
    | s == nullPtr = return Nothing
    | otherwise = Just <$> cstringToText s

attrsToTexts :: Ptr CString -> IO [(Text, Text)]
attrsToTexts attrs =
    do name' <- peek attrs
       case name' of
         _ | name' == nullPtr ->
               return []
         _ ->
             do name'' <- cstringToText name'
                let attrs' = attrs `plusPtr` sizeOf name'
                value' <- peek attrs' 
                value'' <- cstringToText value'
                let attrs'' = attrs' `plusPtr` sizeOf value'
                ((name'', value'') :) <$> attrsToTexts attrs''
                
-- | Compatibility mapper to Data.XML.Types
expatToXml :: Monad m => Conduit ExpatEvent m Event
expatToXml =
    let makeName nsStack name =
            case T.split (== ':') name of
              [prefix, name'] ->
                  let ns = foldl (\mUri nss ->
                                      maybe (Map.lookup (Just prefix) nss)
                                      Just 
                                      mUri
                                 ) Nothing nsStack
                  in Name
                     { nameLocalName = name'
                     , nameNamespace = ns
                     , namePrefix = Just prefix
                     }
              _ ->
                  let ns = foldl (\mUri nss ->
                                      maybe (Map.lookup Nothing nss)
                                      Just 
                                      mUri
                                 ) Nothing nsStack
                  in Name
                     { nameLocalName = name
                     , nameNamespace = ns
                     , namePrefix = Nothing
                     }
        push nsStack (StartElement name attrs) =
            let (nss, attrs') =
                    foldl (\(nss, attrs') (k, v) ->
                               case "xmlns:" `T.isPrefixOf` k of
                                 True ->
                                     let prefix = Just $ T.drop 6 k
                                     in (Map.insert prefix v nss, attrs')
                                 _ | k == "xmlns" ->
                                     (Map.insert Nothing v nss, attrs')
                                 _ ->
                                     (nss, attrs' ++ [(k, v)])
                          ) (Map.empty, []) attrs
                nsStack' = nss : nsStack
                name' = makeName nsStack' name
                attrs'' = map (\(k, v) ->
                               ( makeName nsStack' k
                               , [ContentText v]
                               )
                             ) attrs'
            in return $ StateProducing nsStack' [EventBeginElement name' attrs'']
        push nsStack (EndElement name) =
            let name' = makeName nsStack name
            in return $ StateProducing (tail nsStack) [EventEndElement name']
        push nsStack (CharacterData s) =
            return $ StateProducing nsStack [EventContent $
                                             ContentText s]
        push nsStack expatEvent =
            error $ "Unhandled " ++ show expatEvent
        close _ = return []
    in conduitState [] push close
