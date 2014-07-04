{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

import           Control.Applicative
import           Control.Concurrent.MVar
import           Control.Monad
import           Data.Default                       (def)
import           Data.Maybe
import           GHC.Generics                       (Generic)
import           Graphics.UI.Gtk                    hiding (eventSent)
import           System.Console.CmdArgs             hiding (def)
import           System.Directory                   (doesFileExist)
import           System.IO.Unsafe

import           Drawing
import           GtkUtils
import           Morph

-- | Command-Line configuration options
data CmdLineCfg = CmdLineCfg
  { cfgCsvFile   :: FilePath -- ^ path to elder db (csv-file)
  , cfgGladeFile :: FilePath -- ^ path to glade file
  } deriving (Data, Typeable, Generic)

-- | Read command-line arguments
cmdLineCfg :: IO CmdLineCfg
cmdLineCfg = cmdArgs $ CmdLineCfg
    { cfgCsvFile   = "db.csv" &= name "csv-file" &= explicit &= typFile
                   &= help "path to elder db file (default: 'db.csv')"
    , cfgGladeFile = "gigamesh.glade" &= name "glade-file" &= explicit &= typFile
                   &= help "path to glade file (default: 'gigamesh.glade')"
    } &= program "gigamesh"
      &= summary "Implementation of genetic algorithm as described by Richard Dawkins in 'The Blind Watchmaker'"

-- | 'MVar' to access elder
elder :: MVar (Morph a)
elder = unsafePerformIO $ newMVar def
{-# NOINLINE elder #-}

-- | Entry point for executable
main :: IO ()
main = do
    CmdLineCfg{..} <- cmdLineCfg

    -- load elder db
    dbExists <- doesFileExist cfgCsvFile
    unless dbExists $ writeFile cfgCsvFile "" -- create clean db
    morphDb <- initMorphDb cfgCsvFile

    -- init user interface
    initGUI

    builder <- builderNew
    builderAddFromFile builder cfgGladeFile

    -- setup windows
    mainWindow <- getWindow builder "mainWindow"
    nameWindow <- getWindow builder "nameWindow"

    -- setup drawing areas
    canvs <- setupDrawingAreas builder elder

    -- init combo-box in main window
    combo <- getComboBox builder "comboboxElder"
    comboBoxSetModelText combo
    ix <- comboBoxAppendText combo "New ..."
    comboBoxSetActive combo ix
    elderNames <- morphNames morphDb
    mapM_ (comboBoxAppendText combo) elderNames

    on combo changed $ do
        entry <- fromMaybe "" <$> comboBoxGetActiveText combo
        newElder <- fromMaybe def <$> lookupMorph morphDb entry
        swapMVar elder newElder
        mapM_ widgetQueueDraw canvs

    -- setup windows
    buildSaveElderWindow builder nameWindow combo morphDb
    buildMainWindow builder nameWindow canvs

    widgetShowAll mainWindow
    onDestroy mainWindow mainQuit
    mainGUI

buildSaveElderWindow :: Builder -> Window -> ComboBox -> MorphDb -> IO ()
buildSaveElderWindow builder nameWindow comboBox morphDb = do

    nameEntry <- getEntry builder "nameEntry"
    errorLabel <- getLabel builder "nameErrorLabel"

    buttonSaveName <- getButton builder "buttonSaveName"
    onClicked buttonSaveName $ do

        newName <- entryGetText nameEntry
        newElder <- setName newName <$> readMVar elder

        res <- addMorph morphDb newElder
        case res of
          Left err -> labelSetText errorLabel $ "Could not store elder: " ++ err
          Right _ -> do
            -- redraw
            ix <- comboBoxAppendText comboBox newName
            comboBoxSetActive comboBox ix

            widgetHide nameWindow
            entrySetText nameEntry ""
            labelSetText errorLabel ""

    buttonCancel <- getButton builder "buttonCancelName"
    void $ onClicked buttonCancel $ do
        entrySetText nameEntry ""
        widgetHide nameWindow
        entrySetText nameEntry ""
        labelSetText errorLabel ""

buildMainWindow :: Builder -> Window -> [DrawingArea] -> IO ()
buildMainWindow builder saveWindow canvs = do
    buttonRand <- getButton builder "buttonRandElder"
    onClicked buttonRand $ do
        newElder <- randomMorph
        swapMVar elder newElder
        mapM_ widgetQueueDraw canvs

    buttonClear <- getButton builder "buttonClearElder"
    onClicked buttonClear $ do
        let newElder = def
        swapMVar elder newElder
        mapM_ widgetQueueDraw canvs

    buttonSave <- getButton builder "buttonSaveElder"
    void $ onClicked buttonSave $ windowPresent saveWindow
