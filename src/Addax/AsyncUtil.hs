-- Copyright (C) 2017 Robert Helgesson <robert@rycee.net>
--
-- Addax is free software: you can redistribute it and/or modify it
-- under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- Addax is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with Addax.  If not, see <http://www.gnu.org/licenses/>.

module Addax.AsyncUtil where

import Control.Concurrent.Async (Async, async, withAsync, mapConcurrently)
import Control.Monad.Logger (MonadLoggerIO, LoggingT, askLoggerIO, runLoggingT)
import Control.Monad.Trans (liftIO)

asyncLog :: MonadLoggerIO m => LoggingT IO a -> m (Async a)
asyncLog act =
  do
    logger <- askLoggerIO
    liftIO $ async (runLoggingT act logger)

mapConcurrentlyLog
    :: (Traversable t, MonadLoggerIO m)
    => (a -> LoggingT IO b)
    -> t a
    -> m (t b)
mapConcurrentlyLog act xs =
  do
    logger <- askLoggerIO
    liftIO $ mapConcurrently (\x -> runLoggingT (act x) logger) xs

withAsyncLog :: LoggingT IO a -> (Async a -> LoggingT IO b) -> LoggingT IO b
withAsyncLog a b =
  do
    logger <- askLoggerIO
    liftIO $ withAsync (runLoggingT a logger) (\ x -> runLoggingT (b x) logger)
