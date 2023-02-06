module HaskellWorks.CabalCache.IO.Console
  ( putLn,
    hPutLn,
  ) where

import Control.Exception      (bracket_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Function          (($), (.))
import Prelude                (IO)

import qualified Control.Concurrent.QSem        as IO
import qualified Data.Text.Lazy.IO              as LT
import qualified HaskellWorks.CabalCache.Pretty as PP
import qualified Prettyprinter                  as PP
import qualified Prettyprinter.Render.Text      as PP
import qualified System.IO                      as IO
import qualified System.IO.Unsafe               as IO

sem :: IO.QSem
sem = IO.unsafePerformIO $ IO.newQSem 1
{-# NOINLINE sem #-}

consoleBracket :: IO a -> IO a
consoleBracket = bracket_ (IO.waitQSem sem) (IO.signalQSem sem)

putLn :: MonadIO m => PP.Doc PP.Ann -> m ()
putLn = liftIO . consoleBracket . LT.putStrLn . PP.renderLazy . PP.layoutPretty PP.defaultLayoutOptions { PP.layoutPageWidth = PP.Unbounded }

hPutLn :: MonadIO m => IO.Handle -> PP.Doc PP.Ann -> m ()
hPutLn h = liftIO . consoleBracket . LT.hPutStr h . PP.renderLazy . PP.layoutPretty PP.defaultLayoutOptions { PP.layoutPageWidth = PP.Unbounded }
