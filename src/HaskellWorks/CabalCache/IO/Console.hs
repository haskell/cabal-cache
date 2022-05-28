{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module HaskellWorks.CabalCache.IO.Console
  ( putStrLn
  , print
  , hPutStrLn
  , hPrint
  ) where

import Data.Text  (Text)
import Polysemy   (Sem, Member)
import Prelude    (IO, Show (..), ($), (.))

import qualified Control.Concurrent.QSem as IO
import qualified Data.Text.IO            as T
import qualified Polysemy                as PY
import qualified Polysemy.Resource       as PY
import qualified System.IO               as IO
import qualified System.IO.Unsafe        as IO

sem :: IO.QSem
sem = IO.unsafePerformIO $ IO.newQSem 1
{-# NOINLINE sem #-}

consoleBracket :: ()
  => Member (PY.Embed IO) r
  => Member (PY.Resource) r
  => Sem r a
  -> Sem r a
consoleBracket = PY.bracket_ (PY.embed $ IO.waitQSem sem) (PY.embed $ IO.signalQSem sem)

putStrLn :: ()
  => Member (PY.Embed IO) r
  => Member (PY.Resource) r
  => Text
  -> PY.Sem r ()
putStrLn = consoleBracket . PY.embed . T.putStrLn

print :: ()
  => Member (PY.Embed IO) r
  => Member (PY.Resource) r
  => Show a
  => a
  -> Sem r ()
print = consoleBracket . PY.embed . IO.print

hPutStrLn :: ()
  => Member (PY.Embed IO) r
  => Member (PY.Resource) r
  => IO.Handle
  -> Text
  -> Sem r ()
hPutStrLn h = consoleBracket . PY.embed . T.hPutStrLn h

hPrint :: ()
  => Member (PY.Embed IO) r
  => Member (PY.Resource) r
  => Show a
  => IO.Handle
  -> a
  -> Sem r ()
hPrint h = consoleBracket . PY.embed . IO.hPrint h
