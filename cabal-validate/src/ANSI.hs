-- | ANSI escape sequences.
--
-- This is a stripped-down version of the parts of the @ansi-terminal@ package
-- we use.
--
-- See: <https://gist.github.com/fnky/458719343aabd01cfb17a3a4f7296797>

module ANSI
  ( SGR(..)
  , setSGR
  ) where


-- | Render a single numeric SGR sequence.
rawSGR :: Int -> String
rawSGR code = "\x1b[" <> show code <> "m"

-- | Render a series of `SGR` escape sequences.
setSGR :: [SGR] -> String
setSGR = concat . map renderSGR

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

  | BoldBlack
  | BoldRed
  | BoldGreen
  | BoldYellow
  | BoldBlue
  | BoldMagenta
  | BoldCyan
  | BoldWhite

  | OnBoldBlack
  | OnBoldRed
  | OnBoldGreen
  | OnBoldYellow
  | OnBoldBlue
  | OnBoldMagenta
  | OnBoldCyan
  | OnBoldWhite

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

    BoldBlack -> rawSGR 90
    BoldRed -> rawSGR 91
    BoldGreen -> rawSGR 92
    BoldYellow -> rawSGR 93
    BoldBlue -> rawSGR 94
    BoldMagenta -> rawSGR 95
    BoldCyan -> rawSGR 96
    BoldWhite -> rawSGR 97

    OnBoldBlack -> rawSGR 100
    OnBoldRed -> rawSGR 101
    OnBoldGreen -> rawSGR 102
    OnBoldYellow -> rawSGR 103
    OnBoldBlue -> rawSGR 104
    OnBoldMagenta -> rawSGR 105
    OnBoldCyan -> rawSGR 106
    OnBoldWhite -> rawSGR 107
