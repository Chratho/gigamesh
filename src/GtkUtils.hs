-- | Various helpers wrt the gtk-library
module GtkUtils where

import Data.List                    (intersperse)
import Graphics.Rendering.Cairo     hiding (x,y)
import Graphics.UI.Gtk

----------------------------------------------------------------------------------------------------
-- Gtk helpers

-- | Get 'Button' with corresponding name
getButton :: Builder -> String -> IO Button
getButton builder = builderGetObject builder castToButton

-- | Get 'DrawingArea' with corresponding name
getDrawingArea :: Builder -> String -> IO DrawingArea
getDrawingArea builder = builderGetObject builder castToDrawingArea

-- | Get 'ComboBox' with corresponding name
getComboBox :: Builder -> String -> IO ComboBox
getComboBox builder = builderGetObject builder castToComboBox

-- | Get 'Entry' (input field) with corresponding name
getEntry :: Builder -> String -> IO Entry
getEntry builder = builderGetObject builder castToEntry

-- | Get 'Label' with corresponding name
getLabel :: Builder -> String -> IO Label
getLabel builder = builderGetObject builder castToLabel

-- | Get 'Window' with corresponding name
getWindow :: Builder -> String -> IO Window
getWindow builder = builderGetObject builder castToWindow

----------------------------------------------------------------------------------------------------
-- Cairo drawing helpers

-- Degree helper
type Degrees = Int

-- | Apply a rendering function on a 'DrawingArea'
applyRendering :: DrawingArea -> Render () -> IO ()
applyRendering canvas render = do
    toDraw <- widgetGetDrawWindow canvas
    renderWithDrawable toDraw render

-- | Conversion helper between radian and degree values
degreeUnit :: Double
degreeUnit = 2 * pi / 360

-- | Perform a rendering using the given rotation
withRotation :: Degrees -> Render () -> Render ()
withRotation d act = rotate rad >> act >> rotate (-rad)
  where
    rad = fromIntegral d * degreeUnit

-- | Perform multiple renderings starting from the same location
fromCurrentPoint :: [Render ()] -> Render ()
fromCurrentPoint acts = do
    (x,y) <- getCurrentPoint
    sequence_ $ intersperse (resetPath x y) acts

-- | Finish current rendering and jump to given location
resetPath :: Double -> Double -> Render ()
resetPath x y = stroke >> moveTo x y

-- | Draw line parallel to x-axis
xLine :: Double -> Render ()
xLine v = relLineTo v 0

-- | Draw line parallel to y-axis
yLine :: Double -> Render ()
yLine v = relLineTo 0 v

