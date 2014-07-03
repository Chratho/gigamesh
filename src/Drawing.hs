{-# LANGUAGE RecordWildCards #-}

-- | Module encapluslating actual drawing functionality
module Drawing
  ( setupDrawingAreas
  ) where

import           Control.Concurrent.MVar
import           Control.Monad
import           Graphics.UI.Gtk                    hiding (eventSent)
import           Graphics.Rendering.Cairo
import           Graphics.UI.Gtk.Gdk.Events         (eventSent)

import Morph
import GtkUtils

-- | A particular descendant of the elder
data DescendantMeta a = DescendantMeta
  { mmCanvasId :: String -- ^ DrawingArea's id to render to
  , mmButtonId :: String -- ^ Corresponding Button's id
  , mmIncrFunc :: Int -> (Morph a) -> (Morph a) -- ^ deviation function
  , mmIncr     :: Int    -- ^ increment for deviation function
  }

-- | Deviation functions for all genes defined within a 'Morph'
--
-- The 'Int' within each pair defines the corresponding Gtk-Frame
-- the descending morph will be rendered to.
--
deviationFuncs :: [(Int, Int -> (Morph a) -> (Morph a))]
deviationFuncs = zip [1..] [ addDepthUnits
                           , addLineLengthUnits
                           , addXScaleUnits
                           , addYScaleUnits
                           , addGradientUnits
                           , addDegrees1Units
                           , addDegrees2Units
                           ]

-- | Setup DrawingAreas for the elder morph and all descendants
setupDrawingAreas :: Builder -> MVar (Morph a) -> IO [DrawingArea]
setupDrawingAreas builder elder = do
    -- get drawing areas
    elderCanv <- getDrawingArea builder "canvasElder"
    geneCanvs <- mapM (getDrawingArea builder) (map mmCanvasId geneMetas)
    let allCanvs = elderCanv : geneCanvs

    -- add redraw-event listeners
    addRedrawListener elder elderCanv (flip const) 0 -- "constant deviation" for elder
    zipWithM addGeneRedrawListener geneMetas geneCanvs

    -- init buttons
    forM_ geneMetas $ \DescendantMeta{..} -> do
        geneButton <- getButton builder mmButtonId
        onClicked geneButton $ do
            g <- readMVar elder
            let newElder = mmIncrFunc mmIncr g
            swapMVar elder newElder
            mapM_ widgetQueueDraw (elderCanv : geneCanvs)

    return allCanvs
  where
    geneMetas = [ DescendantMeta
                    ("canvasGene" ++ show i ++ canvKind)
                    ("buttonGene" ++ show i ++ canvKind)
                    func
                    incr
                | (i,func) <- deviationFuncs, (canvKind,incr) <- [ ("Plus",1), ("Minus",(-1)) ]
                ]

    addGeneRedrawListener DescendantMeta{..} canv = addRedrawListener elder canv mmIncrFunc mmIncr

-- | Add a redraw/expose-listener to a 'DrawingArea', specifying a deviation function & increment
addRedrawListener :: MVar (Morph a) -- ^ elder morph
                  -> DrawingArea    -- ^ canvas of corresponding gene
                  -> (Int -> Morph a -> Morph a) -- ^ deviation function
                  -> Int            -- ^ increment for deviation function
                  -> IO ()
addRedrawListener elder canvas func incr = void . onExpose canvas $ \e -> do
    m <- readMVar elder

    (w,h) <- widgetGetSize canvas
    applyRendering canvas $
        prepareDrawingArea >> renderMorph (fromIntegral w) (fromIntegral h) (func incr m)

    return $ eventSent e

-- | Basic preparation of canvas
prepareDrawingArea :: Render ()
prepareDrawingArea = do
    setSourceRGB 1 1 1
    paint
    setSourceRGB 0 0 0
    setLineWidth 2

-- | Render the given morph
renderMorph :: Double    -- ^ width of outer 'DrawingArea'
            -> Double    -- ^ height of outer 'DrawingArea'
            -> Morph a   -- ^ morph to render
            -> Render ()
renderMorph w h Morph{..}
  | mDepth <= 0 = return ()
  | otherwise = do
      moveTo (w/2) yCenter
      scale mXScale mYScale
      yLine (-mLineLength * h)
      drawLevel mDepth
  where
    yCenter = h/2 + h * (mLineLength + (cos (fromIntegral mDegrees1 * degreeUnit) * mLineLength))/2

    drawLevel d
      | 0 == d    = return ()
      | otherwise = drawFork >> stroke
      where
        level = mDepth - d
        lvlDegrees = case level `mod` 4 of
                       0 -> mDegrees1 `div` 2
                       1 -> mDegrees1
                       2 -> mDegrees1 + ((mDegrees2 - mDegrees1) `div` 2)
                       _ -> mDegrees2

        lineSz = (-mLineLength) * (mGradient ^ level) * h

        drawFork = fromCurrentPoint $ [ drawBranch lvlDegrees, drawBranch (-lvlDegrees) ]

        drawBranch degrees = withRotation degrees $ yLine lineSz >> drawLevel (d-1)
