module TUI (
  app,
) where

import State
import Util

import Brick (App (..), BrickEvent (VtyEvent), EventM, Next, Padding (Pad), Widget, attrMap, bg, continue, fg, hBox, hLimit, halt, padLeft, padRight, showFirstCursor, str, vBox, withAttr)
import Brick.Widgets.Border (border)
import Brick.Widgets.Center (center, hCenter)
import Graphics.Vty (Event (EvKey), Key (..), brightBlack, brightGreen, brightRed, defAttr)


attrDef, attrSel, attrActive, attrInactive :: Widget n -> Widget n
attrDef      = withAttr "def"
attrSel      = withAttr "selected"
attrActive   = withAttr "active"
attrInactive = withAttr "inactive"

app :: App State e ()
app = App
  { appDraw         = drawTUI
  , appChooseCursor = showFirstCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = pure
  , appAttrMap      = const . attrMap mempty $
      [ ("selected", bg brightBlack)
      , ("active"  , fg brightGreen)
      , ("inactive", fg brightRed  )
      , ("def"     , defAttr       )
      ]
  }

drawTUI :: State -> [Widget ()]
drawTUI State{ size, grid, pos, convs, useClipboard } =
  (:[]) . center . attrDef . hLimit (mSize + 31) . hBox $
    [ hLimit mSize . drawBox $ drawGuesses grid
    , hLimit 31 . drawBox . vBox  $ map (str . show) convs
           <> [ hBox [str " "]       -- newline
              , hBox [drawClipboard]
              ]
    ]
 where
  mSize :: Int = max 15 (5 * fst size + 4)

  drawBox :: Widget n -> Widget n
  drawBox = border . pad . hCenter

  pad :: Widget n -> Widget n
  pad = padRight (Pad 1) . padLeft (Pad 1)

  drawGuesses :: Grid -> Widget n
  drawGuesses = hBox . imap drawGuess

  drawGuess :: Int -> [MInt] -> Widget n
  drawGuess col = vBox . imap \row x ->
    let attr :: Widget n -> Widget n
        attr = if (col, row) == pos then attrSel else attrDef
     -- Highlight the current focus, but not the border around it.
     in attrDef . pad . attr $ str (show x)

  drawClipboard :: Widget n
  drawClipboard = bool attrInactive attrActive useClipboard . str $
    "C: Copy to clipboard: " <> bool "Off" "On" useClipboard


handleEvent :: State -> BrickEvent () e -> EventM () (Next State)
handleEvent s e = case handleConvertEvent s e of
  Just event -> event
  Nothing    -> case e of
    VtyEvent ve -> case ve of
      EvKey (KChar 'q') [] -> halt s           -- catch fire
      -- manipulating the matrix
      EvKey (KChar '-') [] -> continue $ toggleNeg  s
      EvKey (KChar 'C') [] -> continue $ toggleClipboard s
      EvKey (KChar k)   [] -> continue $ maybe s (`fill` s) (buildMInt k)
      EvKey KRight      [] -> continue $ move East  s
      EvKey KLeft       [] -> continue $ move West  s
      EvKey KUp         [] -> continue $ move North s
      EvKey KDown       [] -> continue $ move South s
      EvKey KBS         [] -> continue $ del  Back  s
      EvKey KDel        [] -> continue $ del  Front s
      _                    -> continue s
    _ -> continue s
