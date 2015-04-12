
module Text.Pandoc.TikZ.GhostScript where

import Text.Pandoc.TikZ.Error (guardIO)

import Control.Monad.Trans.Except
import Control.Monad.Trans.Class (lift)
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

convertPDFtoPNG :: FilePath -> FilePath -> FilePath -> ExceptT String IO ()
convertPDFtoPNG ghostScriptPath pdfFileName pngFileName = do
  let arguments = [ "-sDEVICE=pngalpha"
                  , "-o"
                  , pngFileName
                  , "-r144"
                  , pdfFileName
                  ]
      process = proc ghostScriptPath arguments
  (Nothing, Just _, Just stderrHandle, handle) <-
    guardIO $ createProcess process { std_out = CreatePipe, std_err = CreatePipe }
  exitCode <- lift $ waitForProcess handle
  lift $ removeIfExists pdfFileName
  case exitCode of
    ExitFailure v -> do
      msg <- lift $ hGetContents stderrHandle
      throwE $ "Exit code: " ++ show v ++ ", output: " ++ msg
    ExitSuccess -> return ()

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e
