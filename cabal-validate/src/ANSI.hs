-- | ANSI escape sequences.
--
-- This is a stripped-down version of the parts of the @ansi-terminal@ package
-- we use.
--
-- See: <https://gist.github.com/fnky/458719343aabd01cfb17a3a4f7296797>
module ANSI
  ( SGR (..)
  , setSGR
  ) where

-- | Render a single numeric SGR sequence.
rawSGR :: Int -> String
rawSGR code = "\x1b[" <> show code <> "m"

-- | Render a series of `SGR` escape sequences.
setSGR :: [SGR] -> String
setSGR = concatMap renderSGR

-- | All of the SGR sequences we want to use.
data SGR
  = Reset
  | Bold
  | Dim
  | Italic
  | Underline
  | Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  | Default
  | OnBlack
  | OnRed
  | OnGreen
  | OnYellow
  | OnBlue
  | OnMagenta
  | OnCyan
  | OnWhite
  | OnDefault
  | BrightBlack
  | BrightRed
  | BrightGreen
  | BrightYellow
  | BrightBlue
  | BrightMagenta
  | BrightCyan
  | BrightWhite
  | OnBrightBlack
  | OnBrightRed
  | OnBrightGreen
  | OnBrightYellow
  | OnBrightBlue
  | OnBrightMagenta
  | OnBrightCyan
  | OnBrightWhite
  deriving (Show)

-- Render a single `SGR` sequence.
renderSGR :: SGR -> String
renderSGR code =
  case code of
    Reset -> rawSGR 0
    Bold -> rawSGR 1
    Dim -> rawSGR 2
    Italic -> rawSGR 3
    Underline -> rawSGR 4
    Black -> rawSGR 30
    Red -> rawSGR 31
    Green -> rawSGR 32
    Yellow -> rawSGR 33
    Blue -> rawSGR 34
    Magenta -> rawSGR 35
    Cyan -> rawSGR 36
    White -> rawSGR 37
    Default -> rawSGR 39
    OnBlack -> rawSGR 40
    OnRed -> rawSGR 41
    OnGreen -> rawSGR 42
    OnYellow -> rawSGR 43
    OnBlue -> rawSGR 44
    OnMagenta -> rawSGR 45
    OnCyan -> rawSGR 46
    OnWhite -> rawSGR 47
    OnDefault -> rawSGR 49
    BrightBlack -> rawSGR 90
    BrightRed -> rawSGR 91
    BrightGreen -> rawSGR 92
    BrightYellow -> rawSGR 93
    BrightBlue -> rawSGR 94
    BrightMagenta -> rawSGR 95
    BrightCyan -> rawSGR 96
    BrightWhite -> rawSGR 97
    OnBrightBlack -> rawSGR 100
    OnBrightRed -> rawSGR 101
    OnBrightGreen -> rawSGR 102
    OnBrightYellow -> rawSGR 103
    OnBrightBlue -> rawSGR 104
    OnBrightMagenta -> rawSGR 105
    OnBrightCyan -> rawSGR 106
    OnBrightWhite -> rawSGR 107
