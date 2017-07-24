{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.FixedColumn
-- Copyright   :  (c) 2008 Justin Bogner <mail@justinbogner.com>
-- License     :  BSD3-style (as xmonad)
--
-- Maintainer  :  Justin Bogner <mail@justinbogner.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A layout much like Tall, but using a multiple of a window's minimum
-- resize amount instead of a percentage of screen to decide where to
-- split. This is useful when you usually leave a text editor or
-- terminal in the master pane and like it to be 80 columns wide.
--
-----------------------------------------------------------------------------

module XMonad.Layout.FixedColumn (
    -- * Usage
    -- $usage
        FixedColumn(..)
) where

import Control.Monad (msum)
import Data.Maybe (fromMaybe)
import Graphics.X11.Xlib (Window, rect_width)
import Graphics.X11.Xlib.Extras ( getWMNormalHints
                                , getWindowAttributes
                                , sh_base_size
                                , sh_resize_inc
                                , wa_border_width)

import XMonad.Core (X, LayoutClass(..), fromMessage, io, withDisplay)
import XMonad.Layout (Resize(..), IncMasterN(..), tile)
import XMonad.StackSet as W

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.FixedColumn
--
-- Then edit your @layoutHook@ by adding the FixedColumn layout:
--
-- > myLayout = FixedColumn 1 20 80 10 ||| Full ||| etc..
-- > main = xmonad def { layoutHook = myLayout }
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"

-- | A tiling mode based on preserving a nice fixed width
--   window. Supports 'Shrink', 'Expand' and 'IncMasterN'.
data FixedColumn a = FixedColumn
    { fc_nmaster  :: !Int -- ^ Number of windows in the master pane
    , fc_delta    :: !Int -- ^ 'Resize' by /this/ many columns
    , fc_ncol     :: !Int -- ^ Default column width of master pane
    , fc_fallback :: !Int -- ^ Pixel width of one column, if not a terminal
    } deriving (Read, Show)

instance LayoutClass FixedColumn Window where
    doLayout FixedColumn {..} r s = do
            fws <- mapM (widthCols fc_fallback fc_ncol) ws
            let frac = maximum (take fc_nmaster fws) // rect_width r
                rs   = tile frac r fc_nmaster (length ws)
            return $ (zip ws rs, Nothing)
        where ws     = W.integrate s
              x // y = fromIntegral x / fromIntegral y

    pureMessage FixedColumn {..} m =
            msum [fmap resize     (fromMessage m)
                 ,fmap incmastern (fromMessage m)]
        where resize Shrink
                  = FixedColumn { fc_ncol = max 0 $ fc_ncol - fc_delta, .. }
              resize Expand
                  = FixedColumn { fc_ncol = fc_ncol + fc_delta, .. }
              incmastern (IncMasterN d)
                  = FixedColumn { fc_nmaster = max 0 $ fc_nmaster + d, .. }

    description _ = "FixedColumn"

-- | Determine the width of @w@ given that we would like it to be @n@
--   columns wide, using @inc@ as a resize increment for windows that
--   don't have one
widthCols :: Int -> Int -> Window -> X Int
widthCols inc n w = withDisplay $ \d -> io $ do
    sh <- getWMNormalHints d w
    bw <- fmap (fromIntegral . wa_border_width) $ getWindowAttributes d w
    let widthHint f = f sh >>= return . fromIntegral . fst
        oneCol      = fromMaybe inc $ widthHint sh_resize_inc
        base        = fromMaybe 0 $ widthHint sh_base_size
    return $ 2 * bw + base + n * oneCol
