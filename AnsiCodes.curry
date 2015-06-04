------------------------------------------------------------------------------
--- Library for formatted output on terminals
---
--- Information on ANSI Codes can be found at
--- http://en.wikipedia.org/wiki/ANSI_escape_code
---
--- @author Sebastian Fischer, Björn Peemöller
--- @version March 2015
--- @category general
------------------------------------------------------------------------------

module AnsiCodes
  ( -- cursor movement
    cursorPos
  , cursorHome
  , cursorUp
  , cursorDown
  , cursorFwd
  , cursorBack
  , saveCursor
  , restoreCursor

  -- graphics control
  , clear
  , eraseLine

  -- formatting output
  , normal
  , bold
  , faint
  , italic
  , underline
  , blinkSlow
  , blinkRapid
  , inverse
  , concealed
  , crossedout

  -- foreground color
  , black
  , red
  , green
  , yellow
  , blue
  , cyan
  , magenta
  , white
  , default

  -- background color
  , bgBlack
  , bgRed
  , bgGreen
  , bgYellow
  , bgBlue
  , bgCyan
  , bgMagenta
  , bgWhite
  , bgDefault
  ) where

import List (isSuffixOf)

-- -----------------------------------------------------------------------------
-- Cursor movement
-- -----------------------------------------------------------------------------

--- move cursor to position
cursorPos :: Int -> Int -> String
cursorPos r c = cmd (show r ++ ";" ++ show c ++ "H")

--- move cursor to home position
cursorHome :: String
cursorHome = cmd "H"

--- move cursor n lines up
cursorUp :: Int -> String
cursorUp = moveCursor "A"

--- move cursor n lines down
cursorDown :: Int -> String
cursorDown = moveCursor "B"

--- move cursor n columns forward
cursorFwd :: Int -> String
cursorFwd = moveCursor "C"

--- move cursor n columns backward
cursorBack :: Int -> String
cursorBack = moveCursor "D"

--- save cursor position
saveCursor :: String
saveCursor = cmd "s"

--- restore saved cursor position
restoreCursor :: String
restoreCursor = cmd "u"

-- -----------------------------------------------------------------------------
-- Graphics control
-- -----------------------------------------------------------------------------

--- clear screen
clear :: String
clear = cmd "2J"

--- erase line
eraseLine :: String
eraseLine = cmd "K"

-- -----------------------------------------------------------------------------
-- Text formatting
-- -----------------------------------------------------------------------------

--- Reset formatting to normal formatting
normal :: String -> String
normal = mode 0

--- Bold text
bold :: String -> String
bold = mode 1

--- Faint text
faint :: String -> String
faint = mode 2

--- Italic text
italic :: String -> String
italic = mode 3

--- Underlined text
underline :: String -> String
underline = mode 4

--- Slowly blinking text
blinkSlow :: String -> String
blinkSlow = mode 5

--- rapidly blinking text
blinkRapid :: String -> String
blinkRapid = mode 6

--- Inverse colors
inverse :: String -> String
inverse = mode 7

--- Concealed (invisible) text
concealed :: String -> String
concealed = mode 8

--- Crossed out text
crossedout :: String -> String
crossedout = mode 9

-- -----------------------------------------------------------------------------
-- Foreground color
-- -----------------------------------------------------------------------------

--- Black foreground color
black :: String -> String
black = mode 30

--- Red foreground color
red :: String -> String
red = mode 31

--- Green foreground color
green :: String -> String
green = mode 32

--- Yellow foreground color
yellow :: String -> String
yellow = mode 33

--- Blue foreground color
blue :: String -> String
blue = mode 34

--- Magenta foreground color
magenta :: String -> String
magenta = mode 35

--- Cyan foreground color
cyan :: String -> String
cyan = mode 36

--- White foreground color
white :: String -> String
white = mode 37

--- Default foreground color
default :: String -> String
default = mode 39

-- -----------------------------------------------------------------------------
-- Background color
-- -----------------------------------------------------------------------------

--- Black background color
bgBlack :: String -> String
bgBlack = mode 40

--- Red background color
bgRed :: String -> String
bgRed = mode 41

--- Green background color
bgGreen :: String -> String
bgGreen = mode 42

--- Yellow background color
bgYellow :: String -> String
bgYellow = mode 43

--- Blue background color
bgBlue :: String -> String
bgBlue = mode 44

--- Magenta background color
bgMagenta :: String -> String
bgMagenta = mode 45

--- Cyan background color
bgCyan :: String -> String
bgCyan = mode 46

--- White background color
bgWhite :: String -> String
bgWhite = mode 47

--- Default background color
bgDefault :: String -> String
bgDefault = mode 49

-- -----------------------------------------------------------------------------
-- Helper functions
-- -----------------------------------------------------------------------------

--- Cursor movements
moveCursor :: String -> Int -> String
moveCursor s n = cmd (show n ++ s)

--- Text mode
mode :: Int -> String -> String
mode n s = cmd (show n ++ "m") ++ s ++ if end `isSuffixOf` s then "" else end
  where end = cmd "0m"

--- Create a command using the CSI (control sequence introducer) "\ESC["
cmd :: String -> String
cmd s = '\ESC' : '[' : s
