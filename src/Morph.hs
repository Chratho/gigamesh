{-# LANGUAGE RecordWildCards, FlexibleInstances #-}

-- | Module encapuslating 'Morph'-related datastructures and database access
--
-- Phantom-Types are used for appropriate data-hiding/type-safety.
--
module Morph
  ( initMorphDb
  , randomMorph
  , setName
  , MorphDb(..)
  , NamedMorph
  , Morph (..)
  -- gene helpers
  , addDepthUnits
  , addLineLengthUnits
  , addXScaleUnits
  , addYScaleUnits
  , addGradientUnits
  , addDegrees1Units
  , addDegrees2Units
  ) where

import           Control.Applicative
import           Control.Arrow                      ((&&&))
import           Control.Concurrent.MVar
import qualified Data.ByteString.Lazy.Char8         as LB
import           Data.Csv
import           Data.Default                       (Default(..))
import qualified Data.HashMap.Strict                as HM
import           Data.Maybe                         (fromMaybe)
import           Data.Vector                        (toList)
import           System.IO.Unsafe
import           System.Random

-- | In-memory represetation of morph db
storedMorphs :: MVar (HM.HashMap String (Morph Res))
storedMorphs = unsafePerformIO $ newMVar HM.empty
{-# NOINLINE storedMorphs #-}

-- | Helpers for accessing the database of morphs
data MorphDb = MorphDb
  { addMorph    :: NamedMorph -> IO (Either String ())
  , lookupMorph :: String -> IO (Maybe NamedMorph)
  , morphNames  :: IO [String]
  }

-- | Phantom-type helper
data Res

-- | The central morph datastructure
--
-- A morph consists of an optional name (a 'Morph Res' always has a name set)
-- and multiple genes. Every single gene influences a morph's appearance.
-- From one generation to the next, only a single gene is allowd to change
-- by a small amount.
--
data Morph a = Morph
  { mName_      :: Maybe String -- ^ optional name
  , mDepth      :: Int          -- ^ gene influencing branching-levels
  , mLineLength :: Double       -- ^ gene influecing branch-lengths
  , mXScale     :: Double       -- ^ gene influecing width-scaling
  , mYScale     :: Double       -- ^ gene influecing height-scaling
  , mGradient   :: Double       -- ^ gene influecing decline of branch-length over multiple branch levels
  , mDegrees1   :: Int          -- ^ gene influecing branching-degrees (for branch-levels 1,3,5,...)
  , mDegrees2   :: Int          -- ^ gene influecing branching-degrees (for branch-levels 2,4,6,...)
  }

instance Default (Morph a) where
  def = Morph Nothing 1 (1/15) 1.0 1.0 1 45 90

-- | Change branching-level by one unit
addDepthUnits :: Int -> Morph a -> Morph a
addDepthUnits i m = m { mDepth = mDepth m + i }

-- | Change branch length by one unit
addLineLengthUnits :: Int -> Morph a -> Morph a
addLineLengthUnits i m = m { mLineLength = mLineLength m + (fromIntegral i * 0.02) }

-- | Change width-scalint by one unit
addXScaleUnits :: Int -> Morph a -> Morph a
addXScaleUnits i m = m { mXScale = mXScale m + (fromIntegral i * 0.1) }

-- | Change height-scaling by one unit
addYScaleUnits :: Int -> Morph a -> Morph a
addYScaleUnits i m = m { mYScale = mYScale m + (fromIntegral i * 0.1) }

-- | Change branch-gradient by one unit
addGradientUnits :: Int -> Morph a -> Morph a
addGradientUnits i m = m { mGradient = mGradient m + (fromIntegral i * 0.1) }

-- | Change branching-degrees by one unit
addDegrees1Units :: Int -> Morph a -> Morph a
addDegrees1Units i m = m { mDegrees1 = mDegrees1 m + (i * 5) }

-- | Change branching-degrees by one unit
addDegrees2Units :: Int -> Morph a -> Morph a
addDegrees2Units i m = m { mDegrees2 = mDegrees2 m + (i * 5) }

-- | Generate a random morph
randomMorph :: IO (Morph a)
randomMorph = Morph <$> return Nothing
                    <*> randomRIO (3,12)
                    <*> randomRIO (0.05,0.12)
                    <*> randomRIO (0.4,1.3)
                    <*> randomRIO (0.4,1.3)
                    <*> randomRIO (0.6,0.95)
                    <*> randomRIO (1,360)
                    <*> randomRIO (1,360)

type NamedMorph = Morph Res

-- | Set a name for the corresponding morph, receiving a 'Morph Res'
setName :: String -> Morph a -> NamedMorph
setName nm m = m { mName_ = Just nm }

-- | Phantom type accessor function
mName :: NamedMorph -> String
mName = fromMaybe (error "panic! unresolved mName_ field") . mName_

instance FromRecord NamedMorph where
  parseRecord v = Morph <$> (Just <$> v .! 0)
                        <*> v .! 1
                        <*> v .! 2
                        <*> v .! 3
                        <*> v .! 4
                        <*> v .! 5
                        <*> v .! 6
                        <*> v .! 7

instance ToRecord NamedMorph where
  toRecord g@Morph{..} = record [ toField (mName g)
                                , toField mDepth
                                , toField mLineLength
                                , toField mXScale
                                , toField mYScale
                                , toField mGradient
                                , toField mDegrees1
                                , toField mDegrees2
                                ]

-- | Init database of morphs
initMorphDb :: FilePath -> IO MorphDb
initMorphDb path = do
    morphs <- (HM.fromList . map (mName &&& id)) <$> readFileDb path
    swapMVar storedMorphs morphs
    return $ MorphDb
        (storeMorph path)
        (\name -> readMVar storedMorphs >>= (return . HM.lookup name))
        (readMVar storedMorphs >>= (return . HM.keys))

-- | Read the morph file (currently a simple csv)
readFileDb :: FilePath -> IO [Morph Res]
readFileDb path = do
    csvCont <- LB.pack <$> readFile path
    case decode NoHeader csvCont of
      Left _ | csvCont == LB.empty -> return []
      Left err -> error $ "Cannot load exisitng elders: " ++ err
      Right vec -> return $ toList vec

-- | Try to store a new morph (in-memory + in filesystem)
--
-- The name of the new morph must be unique.
storeMorph :: FilePath -> Morph Res -> IO (Either String ())
storeMorph path morph = modifyMVar storedMorphs $ \db ->
    if mName morph `HM.member` db
      then return (db, Left "Morph with this name already exists.")
      else do
        LB.appendFile path $ encode [morph] -- write to file
        return $ (HM.insert (mName morph) morph db, Right ()) -- update db

