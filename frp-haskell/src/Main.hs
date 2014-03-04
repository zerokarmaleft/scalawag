{-# LANGUAGE RankNTypes #-}

module Main where

import Control.Concurrent
import Control.Event.Handler
import Control.Monad (forever)
import Reactive.Banana
import Reactive.Banana.Frameworks
import System.Exit
import System.IO (BufferMode(..), hSetEcho, hSetBuffering, stdin, stdout)

type World = (Int, Int)

data Command = MoveUp
             | MoveDown
             | MoveLeft
             | MoveRight
             | Unbound
             | Quit
               deriving (Eq)

instance Show Command where
  show MoveUp    = "w"
  show MoveLeft  = "a"
  show MoveDown  = "s"
  show MoveRight = "d"
  show Unbound   = "#"
  show Quit      = "q"

commandFromKey :: Char -> Command
commandFromKey key =
  case key of
    'w' -> MoveUp
    'a' -> MoveLeft
    's' -> MoveDown
    'd' -> MoveRight
    'q' -> Quit
    _   -> Unbound

onKeyEvent :: Command -> IO ()
onKeyEvent cmd =
  if cmd == Quit
  then quitGame
  else return (show cmd) >>= putStr

quitGame :: IO ()
quitGame = do
  putStrLn "\nDone."
  exitSuccess

main :: IO ()
main = do
  -- configure console
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering

  -- create events for key presses and game clock ticks
  (addKeyEvent, fireKeyEvent)   <- newAddHandler
  (addTickEvent, fireTickEvent) <- newAddHandler

  -- create event-propagation and event-handling network
  let networkDesc :: forall t. Frameworks t => Moment t ()
      networkDesc = do
        keyEvents  <- fromAddHandler $ addKeyEvent
        tickEvents <- fromAddHandler $ addTickEvent

        reactimate $ fmap onKeyEvent keyEvents
        reactimate $ fmap putStr tickEvents

  network <- compile networkDesc
  actuate network

  -- local loop to capture game clock tick events on auxiliary thread
  forkIO $ forever $ do
    threadDelay (10^5)
    fireTickEvent "."

  -- local loop to capture key events on main thread
  -- (quitting will kill the game clock thread)
  forever $ do
    c <- getChar
    fireKeyEvent $ commandFromKey c
