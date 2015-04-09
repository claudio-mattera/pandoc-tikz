
module Text.Pandoc.TikZ.GhostScript where

import System.Process (proc,
                       createProcess,
                       waitForProcess,
                       StdStream(CreatePipe),
                       std_out,
                       std_err)
import System.IO (hGetContents)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))

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
  case exitCode of
    ExitFailure v -> do
      msg <- hGetContents stderrHandle
      error $ "Exit code: " ++ show v ++ ", output: " ++ msg
    ExitSuccess -> return $ Right ()
