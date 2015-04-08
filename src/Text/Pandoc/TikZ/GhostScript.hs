
module Text.Pandoc.TikZ.GhostScript where

import System.Process (proc, createProcess, waitForProcess)
import System.IO (hGetContents)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))

convertPDFtoPNG :: FilePath -> FilePath -> IO (Either String ())
convertPDFtoPNG pdfFileName pngFileName = do
  let arguments = [ "-sDEVICE=pngalpha"
                  , "-o"
                  , pngFileName
                  , "-r144"
                  , pdfFileName
                  ]
      gsProgram = undefined
      process = proc gsProgram arguments
  (Nothing, Nothing, Just stderrHandle, handle) <- createProcess process
  exitCode <- waitForProcess handle
  case exitCode of
    ExitFailure v -> do
      msg <- hGetContents stderrHandle
      error $ "Exit code: " ++ show v ++ ", output: " ++ msg
    ExitSuccess -> return $ Right ()
