
module Text.Pandoc.TikZ.GhostScript where

import System.Process (proc,
                       createProcess,
                       waitForProcess,
                       StdStream(CreatePipe),
                       std_out,
                       std_err)
import System.IO (hGetContents)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))

import Prelude hiding (catch)
import System.Directory (removeFile)
import Control.Exception (catch, throwIO)
import System.IO.Error (isDoesNotExistError)

convertPDFtoPNG :: FilePath -> FilePath -> FilePath -> IO (Either String ())
convertPDFtoPNG ghostScriptPath pdfFileName pngFileName = do
  let arguments = [ "-sDEVICE=pngalpha"
                  , "-o"
                  , pngFileName
                  , "-r144"
                  , pdfFileName
                  ]
      process = proc ghostScriptPath arguments
  (Nothing, Just _, Just stderrHandle, handle) <-
    createProcess process { std_out = CreatePipe, std_err = CreatePipe }
  exitCode <- waitForProcess handle
  removeIfExists pdfFileName
  case exitCode of
    ExitFailure v -> do
      msg <- hGetContents stderrHandle
      error $ "Exit code: " ++ show v ++ ", output: " ++ msg
    ExitSuccess -> return $ Right ()

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e
