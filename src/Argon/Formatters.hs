module Argon.Formatters (bareTextFormatter, coloredTextFormatter) where

import System.Console.ANSI
import Text.Printf (printf)

import Argon.Loc
import Argon.Types

bareTextFormatter :: [(FilePath, AnalysisResult)] -> [String]
bareTextFormatter =
  formatResult
    id
    ("\terror: " ++)
    (\(CC (l, func, cc)) -> printf "\t%s %s - %d" (locToString l) func cc)

coloredTextFormatter :: [(FilePath, AnalysisResult)] -> [String]
coloredTextFormatter =
  formatResult
    (\name -> bold ++ name ++ reset)
    (printf "\t%serror%s: %s" (fore Red) reset)
    ( \(CC (l, func, cc)) ->
        printf
          "\t%s %s - %s"
          (locToString l)
          (coloredFunc func l)
          (coloredRank cc)
    )

-- | ANSI bold color
bold :: String
bold = setSGRCode [SetConsoleIntensity BoldIntensity]

-- | Make a ANSI foreground color sequence
fore :: Color -> String
fore color = setSGRCode [SetColor Foreground Dull color]

-- | ANSI sequence for reset
reset :: String
reset = setSGRCode []

coloredFunc :: String -> Loc -> String
coloredFunc f (_, c) = fore color ++ f ++ reset
  where
    color = if c == 1 then Cyan else Magenta

coloredRank :: Int -> String
coloredRank c = printf "%s%s (%d)%s" (fore color) rank c reset
  where
    (color, rank)
      | c <= 5 = (Green, "A")
      | c <= 10 = (Yellow, "B")
      | otherwise = (Red, "C")

formatResult
  :: (String -> String)
  -- ^ The header formatter
  -> (String -> String)
  -- ^ The error formatter
  -> (ComplexityBlock -> String)
  -- ^ The single line formatter
  -> [(FilePath, AnalysisResult)]
  -> [String]
formatResult header errorF singleF = concatMap $ \case
  (path, Left err) -> [header path, errorF err]
  (path, Right rs) -> header path : (map singleF rs)
