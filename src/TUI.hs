module TUI (
  app,
) where

import State
import Util

import Brick (App (..), BrickEvent (VtyEvent), EventM, Padding (Pad), Widget, attrMap, bg, fg, gets, hBox, hLimit, halt, modify, padLeft, padRight, showFirstCursor, str, vBox, withAttr)
import Brick.AttrMap (attrName)
import Brick.Widgets.Border (border)
import Brick.Widgets.Center (center, hCenter)
import Control.Arrow (first)
import GHC.Exts (toList)
import Graphics.Vty (Event (EvKey), Key (..), brightBlack, brightGreen, brightRed, defAttr)

attrDef, attrSel, attrActive, attrInactive :: Widget n -> Widget n
attrDef      = withAttr $ attrName "def"
attrSel      = withAttr $ attrName "selected"
attrActive   = withAttr $ attrName "active"
attrInactive = withAttr $ attrName "inactive"

app :: App State e ()
app = App
  { appDraw         = drawTUI
  , appChooseCursor = showFirstCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = pure ()
  , appAttrMap      = const . attrMap defAttr . map (first attrName) $
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
    , hLimit 31 . drawBox . vBox  $ map (str . convToString) (toList convs)
                                 <> [ hBox [str " "]       -- newline
                                    , hBox [drawClipboard]
                                    ]
    ]
 where
  mSize :: Int = max 15 (4 * snd size)

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


handleEvent :: BrickEvent () e -> EventM () State ()
handleEvent e = do
  gets convs >>= \c -> handleConvertEvent c e
  case e of
    VtyEvent ve -> case ve of
      EvKey (KChar 'q') [] -> halt        -- catch fire
      _ -> modify case ve of              -- again for modify
        EvKey (KChar '-') [] -> toggleNeg
        EvKey (KChar 'C') [] -> toggleClipboard
        EvKey (KChar k)   [] -> maybe id fill (buildMInt k)
        EvKey KRight      [] -> move East
        EvKey KLeft       [] -> move West
        EvKey KUp         [] -> move North
        EvKey KDown       [] -> move South
        EvKey KBS         [] -> del  Back
        EvKey KDel        [] -> del  Front
        _                    -> id
    _ -> pure ()
