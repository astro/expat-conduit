{-# LANGUAGE ForeignFunctionInterface #-}
module Text.XML.Expat.Conduit.Internal where

import Foreign
import Foreign.C.String
import Foreign.C.Types


type XMLParser = Ptr ()

foreign import ccall unsafe "expat.h XML_ParserCreate"
    xmlParserCreate :: CString -> IO XMLParser
                        
-- actually an enum
type XMLStatus = CInt
                       
-- | Most performance-critical function, but needs to be safe as it
-- calls back into Haskell
foreign import ccall safe "expat.h XML_Parse"
    xmlParse :: XMLParser -> CString -> CInt -> CInt -> IO XMLStatus
                
foreign import ccall unsafe "expat.h XML_ParserFree"
    xmlParserFree :: XMLParser -> IO ()
                     
type UserData = ()

type XMLStartElementHandler = Ptr UserData -> CString -> Ptr CString -> IO ()
foreign import ccall "wrapper"
    mkStartElementHandler :: XMLStartElementHandler -> IO (FunPtr XMLStartElementHandler)
foreign import ccall unsafe "expat.h XML_SetStartElementHandler"
    xmlSetStartElementHandler :: XMLParser -> FunPtr XMLStartElementHandler -> IO ()

type XMLEndElementHandler = Ptr UserData -> CString -> IO ()
foreign import ccall "wrapper"
    mkEndElementHandler :: XMLEndElementHandler -> IO (FunPtr XMLEndElementHandler)
foreign import ccall unsafe "expat.h XML_SetEndElementHandler"
    xmlSetEndElementHandler :: XMLParser -> FunPtr XMLEndElementHandler -> IO ()

type XMLCharacterDataHandler = Ptr UserData -> CString -> CInt -> IO ()
foreign import ccall "wrapper"
    mkCharacterDataHandler :: XMLCharacterDataHandler -> IO (FunPtr XMLCharacterDataHandler)
foreign import ccall unsafe "expat.h XML_SetCharacterDataHandler"
    xmlSetCharacterDataHandler :: XMLParser -> FunPtr XMLCharacterDataHandler -> IO ()

type XMLProcessingInstructionHandler = Ptr UserData -> CString -> CString -> IO ()
foreign import ccall "wrapper"
    mkProcessingInstructionHandler :: XMLProcessingInstructionHandler -> IO (FunPtr XMLProcessingInstructionHandler)
foreign import ccall unsafe "expat.h XML_SetProcessingInstructionHandler"
    xmlSetProcessingInstructionHandler :: XMLParser -> FunPtr XMLProcessingInstructionHandler -> IO ()

type XMLCommentHandler = Ptr UserData -> CString -> IO ()
foreign import ccall "wrapper"
    mkCommentHandler :: XMLCommentHandler -> IO (FunPtr XMLCommentHandler)
foreign import ccall unsafe "expat.h XML_SetCommentHandler"
    xmlSetCommentHandler :: XMLParser -> FunPtr XMLCommentHandler -> IO ()

type XMLStartCdataSectionHandler = Ptr UserData -> IO ()
foreign import ccall "wrapper"
    mkStartCdataSectionHandler :: XMLStartCdataSectionHandler -> IO (FunPtr XMLStartCdataSectionHandler)
foreign import ccall unsafe "expat.h XML_SetStartCdataSectionHandler"
    xmlSetStartCdataSectionHandler :: XMLParser -> FunPtr XMLStartCdataSectionHandler -> IO ()

type XMLEndCdataSectionHandler = Ptr UserData -> IO ()
foreign import ccall "wrapper"
    mkEndCdataSectionHandler :: XMLEndCdataSectionHandler -> IO (FunPtr XMLEndCdataSectionHandler)
foreign import ccall unsafe "expat.h XML_SetEndCdataSectionHandler"
    xmlSetEndCdataSectionHandler :: XMLParser -> FunPtr XMLEndCdataSectionHandler -> IO ()

type XMLStartDoctypeDeclHandler = Ptr UserData -> CString -> CString -> CString -> CInt -> IO ()
foreign import ccall "wrapper"
    mkStartDoctypeDeclHandler :: XMLStartDoctypeDeclHandler -> IO (FunPtr XMLStartDoctypeDeclHandler)
foreign import ccall unsafe "expat.h XML_SetStartDoctypeDeclHandler"
    xmlSetStartDoctypeDeclHandler :: XMLParser -> FunPtr XMLStartDoctypeDeclHandler -> IO ()

type XMLEndDoctypeDeclHandler = Ptr UserData -> IO ()
foreign import ccall "wrapper"
    mkEndDoctypeDeclHandler :: XMLEndDoctypeDeclHandler -> IO (FunPtr XMLEndDoctypeDeclHandler)
foreign import ccall unsafe "expat.h XML_SetEndDoctypeDeclHandler"
    xmlSetEndDoctypeDeclHandler :: XMLParser -> FunPtr XMLEndDoctypeDeclHandler -> IO ()

type XMLEntityDeclHandler = Ptr UserData -> CString -> CInt -> CString -> CInt -> CString -> CString -> CString -> CString -> IO ()
foreign import ccall "wrapper"
    mkEntityDeclHandler :: XMLEntityDeclHandler -> IO (FunPtr XMLEntityDeclHandler)
foreign import ccall unsafe "expat.h XML_SetEntityDeclHandler"
    xmlSetEntityDeclHandler :: XMLParser -> FunPtr XMLEntityDeclHandler -> IO ()

type XMLNotationDeclHandler = Ptr UserData -> CString -> CString -> CString -> CString -> IO ()
foreign import ccall "wrapper"
    mkNotationDeclHandler :: XMLNotationDeclHandler -> IO (FunPtr XMLNotationDeclHandler)
foreign import ccall unsafe "expat.h XML_SetNotationDeclHandler"
    xmlSetNotationDeclHandler :: XMLParser -> FunPtr XMLNotationDeclHandler -> IO ()
