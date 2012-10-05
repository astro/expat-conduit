{-# LANGUAGE OverloadedStrings, TupleSections, DeriveDataTypeable, TypeFamilies #-}
module Text.XML.Expat.Conduit where

import Data.Conduit
import Control.Monad.Trans
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.XML.Types hiding (doctypeName)
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Data.Default
import Control.Concurrent hiding (yield)
import Control.Concurrent.Chan
import Control.Applicative
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Map as Map
import Control.Exception (Exception)
import Data.Typeable (Typeable)
import Control.Monad.Trans.Resource (register)


import Text.XML.Expat.Conduit.Internal

data ExpatEvent = StartElement Text [(Text, Text)]
                | EndElement Text
                | CharacterData Text
                | ProcessingInstruction Text Text
                | Comment Text
                | StartCdataSection
                | EndCdataSection
                | StartDoctypeDecl Text (Maybe Text) (Maybe Text) Bool
                | EndDoctypeDecl
                -- EntityDecl
                  deriving (Eq, Show)

data ExpatError = ExpatError String
                  deriving (Show, Read, Eq, Typeable)

instance Exception ExpatError


data ParseSettings = ParseSettings

instance Default ParseSettings where
    def = ParseSettings


parseBytes :: (MonadThrow m, MonadResource m) =>
              ParseSettings ->
              Conduit B.ByteString m ExpatEvent
parseBytes _ =
  parseBytes' =$= throwErrors
      where throwErrors = 
                await >>=
                maybe (return ())
                (either (lift . monadThrow) $ \event -> 
                     do yield event
                        throwErrors
                )
              
parseBytes' :: MonadResource m =>
               Conduit B.ByteString m (Either ExpatError ExpatEvent)
parseBytes' =
    ioProcess $ \recvInput sendOutput -> 
    runResourceT $ do
      parser <- liftIO $
                xmlParserCreate nullPtr
      _ <- register $
           liftIO $
           xmlParserFree parser
                      
      let sendEvent = sendOutput . Just . Right
          sendError = sendOutput . Just . Left
      -- SAX Handlers
      startElementHandler <-
          liftIO $
          mkStartElementHandler $ \_ name attrs ->
          StartElement <$>
          cstringToText name <*>
          attrsToTexts attrs >>=
          sendEvent
      endElementHandler <-
          liftIO $
          mkEndElementHandler $ \_ name ->
          cstringToText name >>=
          sendEvent . EndElement
      characterDataHandler <-
          liftIO $
          mkCharacterDataHandler $ \_ buf len ->
          decodeUtf8 <$> B.packCStringLen (buf, fromIntegral len) >>=
          sendEvent . CharacterData
      processingInstructionHandler <-
          liftIO $
          mkProcessingInstructionHandler $ \_ target data_ ->
          ProcessingInstruction <$>
          cstringToText target <*>
          cstringToText data_ >>=
          sendEvent
      commentHandler <-
          liftIO $
          mkCommentHandler $ \_ s ->
          cstringToText s >>=
          sendEvent . Comment
      startCdataSectionHandler <-
          liftIO $
          mkStartCdataSectionHandler $ \_ ->
          sendEvent StartCdataSection
      endCdataSectionHandler <-
          liftIO $
          mkEndCdataSectionHandler $ \_ ->
          sendEvent EndCdataSection
      startDoctypeDeclHandler <-
          liftIO $
          mkStartDoctypeDeclHandler $ \_ doctypeName sysid pubid hasInternalSubset ->
          StartDoctypeDecl <$>
          cstringToText doctypeName <*>
          cstringToMaybeText sysid <*>
          cstringToMaybeText pubid <*>
          (pure $ hasInternalSubset /= 0) >>=
          sendEvent
      endDoctypeDeclHandler <-
          liftIO $
          mkEndDoctypeDeclHandler $ \_ ->
          sendEvent EndDoctypeDecl

      let setHandler :: MonadResource m => 
                        (XMLParser -> FunPtr a -> IO ()) -> FunPtr a -> m ()
          setHandler m f =
              do liftIO $ m parser f
                 _ <- register $ freeHaskellFunPtr f
                 return ()
      setHandler xmlSetStartElementHandler startElementHandler
      setHandler xmlSetEndElementHandler endElementHandler
      setHandler xmlSetCharacterDataHandler characterDataHandler
      setHandler xmlSetProcessingInstructionHandler processingInstructionHandler
      setHandler xmlSetCommentHandler commentHandler
      setHandler xmlSetStartCdataSectionHandler startCdataSectionHandler
      setHandler xmlSetEndCdataSectionHandler endCdataSectionHandler
      setHandler xmlSetStartDoctypeDeclHandler startDoctypeDeclHandler
      setHandler xmlSetEndDoctypeDeclHandler endDoctypeDeclHandler
          
      let loop = do
            mData <- recvInput
            status <-
              case mData of
                -- Push data
                Just buf ->
                    B.useAsCStringLen buf $ \(str, len) ->
                    xmlParse parser str (fromIntegral len) 0
                -- EOF
                Nothing ->
                    xmlParse parser nullPtr 0 1
            case status of
              1 ->
                  return ()
              _ ->
                  xmlGetErrorCode parser >>=
                  xmlErrorString >>=
                  (ExpatError . BC.unpack <$>) . B.packCString >>=
                  sendError
                                
            -- Events over for now, send next input
            sendOutput Nothing
                                   
            case mData of
              Just _ ->
                  -- Loop
                  loop
              Nothing ->
                  return ()
      liftIO loop
      
ioProcess :: MonadIO m =>
             (IO (Maybe a) -> (Maybe b -> IO ()) -> IO ()) -> Conduit a m b
ioProcess f =
    do input <- liftIO newChan
       output <- liftIO newChan
       
       let recvInput =
             readChan input
           sendOutput =
             writeChan output
       _ <- liftIO $
            forkIO $ 
            f recvInput sendOutput
              
       let consumeOutput =
             do mOutput <- liftIO $ readChan output
                case mOutput of
                  Just output' ->
                      do yield output'
                         -- Loop
                         consumeOutput
                  Nothing ->
                      return ()
           sendInput =
             do mInput <- await
                liftIO $ writeChan input mInput
                
                consumeOutput
                
                case mInput of
                  Just _ ->
                      -- Loop
                      sendInput
                  Nothing ->
                      -- Done
                      return ()
       sendInput

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
                
                
type NamespaceStack = [Map.Map (Maybe Text) Text]
                
-- | Compatibility mapper to Data.XML.Types
expatToXml :: Monad m => Conduit ExpatEvent m Event
expatToXml = do
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
                     
        loop :: Monad m => NamespaceStack -> Conduit ExpatEvent m Event
        loop nsStack =
          do mExpatEvent <- await
             case mExpatEvent of
               Nothing ->
                   -- nsStack should be [] at this point
                   return ()
               Just expatEvent ->
                   mapEvent nsStack expatEvent >>=
                   loop
                   
        mapEvent nsStack (StartElement name attrs) =
            do let (nss', attrs'') =
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
                   nsStack' = nss' : nsStack
                   name' = makeName nsStack' name
                   attrs''' = map (\(k, v) ->
                                       (makeName nsStack' k, [ContentText v])
                                  ) attrs''
               yield $ EventBeginElement name' attrs'''
               return nsStack'
        mapEvent nsStack (EndElement name) =
            do let name' = makeName nsStack name
               yield $ EventEndElement name'
               return $ tail nsStack
        mapEvent nsStack (CharacterData s) =
            do yield $ EventContent $ ContentText s
               return nsStack
        mapEvent nsStack (ProcessingInstruction target data_) =
            do yield $ EventInstruction $ Instruction target data_
               return nsStack
        mapEvent nsStack (Comment s) =
            do yield $ EventComment s
               return nsStack
        mapEvent nsStack StartCdataSection =
            do let consumeCdata =
                       do mExpatEvent <- await
                          case mExpatEvent of
                            Just (CharacterData s) ->
                                do yield $ EventCDATA s
                                   consumeCdata
                            Just EndCdataSection ->
                                return ()
                            _ ->
                                error "Unterminated CDATA section"
               consumeCdata
               return nsStack
        mapEvent nsStack EndCdataSection =
            -- ignore to mute compiler warnings
            return nsStack
        mapEvent nsStack (StartDoctypeDecl doctypeName mSysId mPubId _) =
            do let externalId =
                       case (mSysId, mPubId) of
                         (Just sysId, Just pubId) ->
                             Just $ PublicID sysId pubId
                         (Just sysId, _) ->
                             Just $ SystemID sysId
                         (_, _) ->
                             Nothing
               yield $ EventBeginDoctype doctypeName externalId
               return nsStack
        mapEvent nsStack EndDoctypeDecl =
            do yield EventEndDoctype
               return nsStack

    yield EventBeginDocument
    loop []
    yield EventEndDocument
   