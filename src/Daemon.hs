module Daemon
    () where

import System.INotify

runDaemon :: IO ()
runDaemon = do
    inotify <- initINotify
    killINotify inotify
