{-# LANGUAGE CPP #-}

module Argon.Parser (LModule, analyze, parseModule)
where

import Control.Exception qualified as E
import Control.Monad (void)

import GHC.LanguageExtensions qualified as GHC

import Argon.Loc
import Argon.Preprocess
import Argon.Types
import Argon.Visitor (funcsCC)
import Control.Monad.IO.Class (liftIO)
import GHC.Data.StringBuffer qualified as GHC
import GHC.Driver.Config.Parser qualified as GHC
import GHC.Parser qualified
import GHC.Parser.Header qualified as GHC
import GHC.Parser.Lexer qualified as GHC
import GHC.Utils.Error (MessageClass (..))
import GHC.Utils.Logger qualified as GHC
import qualified GHC.Types.SrcLoc as GHC
import qualified Language.Haskell.Syntax as GHC
import qualified GHC.Hs.Extension as GHC
import qualified GHC.Driver.DynFlags as GHC
import qualified GHC.Types.Error as GHC
import Language.Haskell.GhclibParserEx.GHC.Parser qualified as GHC.Parser
import qualified GHC.Utils.Outputable as GHC
import qualified GHC.Driver.Ppr as GHC
import Data.Maybe (mapMaybe)
import qualified Language.Haskell.GhclibParserEx.GHC.Driver.Session as GhclibParserEx

-- | Type synonym for a syntax node representing a module tagged with a
--   'SrcSpan'
type LModule = GHC.Located (GHC.HsModule GHC.GhcPs)

-- | Parse the code in the given filename and compute cyclomatic complexity for
--   every function binding.
analyze
  :: Config
  -- ^ Configuration options
  -> FilePath
  -- ^ The filename corresponding to the source code
  -> IO (FilePath, AnalysisResult)
analyze conf file = do
  parseResult <-
    ( do
        result <- parseModule conf file
        E.evaluate result
      )
      `E.catch` handleExc
  let analysis = case parseResult of
        Left err -> Left err
        Right ast -> Right $ funcsCC ast
  return (file, analysis)

handleExc :: E.SomeException -> IO (Either String LModule)
handleExc = return . Left . show

-- | Parse a module with the default instructions for the C pre-processor
--   Only the includes directory is taken from the config
parseModule :: Config -> FilePath -> IO (Either String LModule)
parseModule conf =
  parseModuleWithCpp conf $
    defaultCppOptions
      { cppInclude = includeDirs conf
      , cppFile = headers conf
      }

-- | Parse a module with specific instructions for the C pre-processor.
parseModuleWithCpp
  :: Config
  -> CppOptions
  -> FilePath
  -> IO (Either String LModule)
parseModuleWithCpp conf cppOptions file = do
    str <- readFile file
    eDflags <- setExtensions (mapMaybe GhclibParserEx.readExtension conf.exts, []) file str
    case eDflags of
      Left err -> error err
      Right dflags -> do
        let useCpp = GHC.xopt GHC.Cpp dflags
        str' <-
          if useCpp
            then getPreprocessedSrc cppOptions file str
            else pure str
        eDflags' <- setExtensions (mapMaybe GhclibParserEx.readExtension conf.exts, []) file str'
        case eDflags' of
          Left err -> error err
          Right dflags' -> do
            return $
              case parseCode dflags' file str' of
                GHC.PFailed ps ->
                  Left $
                    tagMsg
                      (srcSpanToLoc $ GHC.mkSrcSpanPs (GHC.last_loc ps))
                      (GHC.showSDoc dflags (GHC.ppr $ GHC.getPsMessages ps))
                GHC.POk _ pmod -> Right pmod

parseCode :: GHC.DynFlags -> FilePath -> String -> GHC.ParseResult LModule
parseCode flags file = GHC.Parser.parseFile file flags

-- runParser :: GHC.P a -> GHC.DynFlags -> FilePath -> String -> GHC.ParseResult a
-- runParser parser flags filename str = GHC.unP parser parseState
--   where
--     location = GHC.mkRealSrcLoc (GHC.mkFastString filename) 1 1
--     buffer = GHC.stringToStringBuffer str
--     parseState = GHC.initParserState (GHC.initParserOpts flags) buffer location

-- initDynFlags :: GHC.GhcMonad m => Config -> FilePath -> m GHC.DynFlags
-- initDynFlags conf file = do
--   dflags0 <- GHC.getSessionDynFlags
--   (dflags1, _, _) <-
--     GHC.parseDynamicFlagsCmdLine
--       dflags0
--       [GHC.L GHC.noSrcSpan ("-X" ++ e) | e <- exts conf]
--   src_opts <- GHC.liftIO $ GHC.getOptionsFromFile (GHC.initParserOpts dflags1) file
--   (dflags2, _, _) <- GHC.parseDynamicFilePragma dflags1 (snd src_opts)
--   GHC.pushLogHookM (const customLogAction)
--   -- let dflags3 = dflags2 { GHC.log_action = customLogAction }
--   void $ GHC.setSessionDynFlags dflags2
--   return dflags2

customLogAction :: GHC.LogAction
customLogAction logFlags (MCDiagnostic severity _ _) srcSpan m =
  case severity of
    GHC.SevError -> throwError
    _ -> return ()
  where
    throwError =
      E.throwIO $
        GhcParseError
          (srcSpanToLoc srcSpan)
          (GHC.renderWithContext (GHC.log_default_user_context logFlags) m)
customLogAction _ _ _ _ = error "impossible?"
