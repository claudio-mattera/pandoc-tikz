{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, TypeFamilies #-}

-- Taken from http://chromaticleaves.com/posts/guard-io-with-errort.html

module Text.Pandoc.TikZ.Error (guardIO) where

import Prelude hiding (catch)
import Control.Exception            (IOException)
import Control.Exception.Lifted  (catch)
import Control.Monad.Trans.Control
import Control.Monad.Error          ( ErrorT
                                     , Monad
                                     , MonadIO
                                     , MonadError
                                     , liftIO
                                     , runErrorT
                                     , throwError
                                     )

guardIO :: (MonadBaseControl IO m, MonadIO m, MonadError String m) => IO a -> m a
guardIO action =
  liftIO action `catch` \e -> throwError $ show (e :: IOException)
