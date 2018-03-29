module DataFiles
    ( getDataFileName
    ) where

import qualified Paths_cl_website_tool

import           Path                  ( (</>), parseAbsDir, parseRelFile, toFilePath )
import           System.Environment    ( lookupEnv )

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
    maybeDataFileBaseDir <- lookupEnv "DATAFILE_BASE_DIR"
    case maybeDataFileBaseDir of
        Nothing              -> Paths_cl_website_tool.getDataFileName name
        Just content -> do
            dataFileBaseDir <- parseAbsDir content
            file <- parseRelFile name
            return $! toFilePath (dataFileBaseDir </> file)
