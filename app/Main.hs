module Main where

import Pipes
import Pipes.Safe (runSafeT)
import qualified Pipes.Prelude as P
import Control.Monad (forM_)
import qualified Options.Applicative as Opt

import Argon
import Options.Applicative ((<**>))
import qualified Options.Applicative.NonEmpty as Opt
import Data.List.NonEmpty (NonEmpty)
import Control.Applicative ((<|>))
import Data.Maybe (maybeToList)


parseArgon :: Opt.Parser Argon
parseArgon = Argon <$> parseConfig <*> parsePaths
  where
    parseConfig :: Opt.Parser Config
    parseConfig = Config <$> minComplexity <*> exts <*> headers <*> includeDirs <*> outputMode
      where
        minComplexity = Opt.option Opt.auto
          (Opt.long "min"
            <> Opt.short 'm'
            <> Opt.metavar "N"
            <> Opt.value 1
            <> Opt.showDefault
            <> Opt.help "the minimum complexity to show in results")

        -- HACK: this should rather be parsed into a separate type, and
        -- constructed from there. For now we overload the use of the field
        -- since the types are (coincidentally) the same
        exts = fmap maybeToList . Opt.optional $ Opt.strOption
          (Opt.long "cabal-file"
            <> Opt.metavar "PATH"
            <> Opt.help "path to Cabal main file")

        headers = Opt.many $ Opt.strOption
          (Opt.long "cabal-macros"
            <> Opt.metavar "PATH"
            <> Opt.help "Cabal header file with versions macros")

        includeDirs = Opt.many $ Opt.strOption
          (Opt.long "include-dir"
            <> Opt.short 'I'
            <> Opt.metavar "PATH"
            <> Opt.help "additional directory with header files")

        outputMode = json <|> noColor
          where
            json = Opt.flag' JSON
              (Opt.long "json"
                <> Opt.short 'j'
                <> Opt.help "results are serialized to JSON")
            noColor = Opt.flag Colored BareText
              (Opt.long "no-color"
                <> Opt.help "results are not colored")

    parsePaths :: Opt.Parser (NonEmpty FilePath)
    parsePaths = Opt.some1 path

    path :: Opt.Parser FilePath
    path = Opt.argument Opt.str (Opt.metavar "FILE")

main :: IO ()
main = do
    argon' <- Opt.execParser opts
    exts <- concat <$> traverse parseExts argon'.config.exts
    let argon = argon' { config = argon'.config { exts = exts } }
    forM_ argon.paths $ \path -> do
        let source = allFiles path
                  >-> P.mapM (liftIO . analyze argon.config)
                  >-> P.map (filterResults argon.config)
                  >-> P.filter filterNulls
        runSafeT $ runEffect $ exportStream argon.config source
  where
    opts =
      Opt.info
        (parseArgon <**> Opt.helper)
        Opt.fullDesc

