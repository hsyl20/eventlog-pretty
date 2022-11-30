{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}

module Main where

import GHC.RTS.Events
import System.Environment
import Control.Monad
import System.IO
import Data.Text (Text)
import Data.Maybe
import qualified Data.Map as Map
import Data.Map (Map)


main :: IO ()
main = do
  putStrLn "Hello, Fast Haskell!"

  getArgs >>= \case
    [f] -> readEventLogFromFile f >>= \case
      Left err  -> putStrLn ("Couldn't parse eventlog: " ++ err)
      Right log -> parseEvents log
    _ -> putStrLn "Missing argument: eventlog file"

parseEvents :: EventLog -> IO ()
parseEvents (EventLog header (Data events)) = do
  putStrLn "Rendering the log..."

  withFile "output.html" WriteMode \h -> do
    let o = hPutStrLn h
    
    -- strLn (show header)

    o "<html>"
    o "<head>"
    o "</head>"
    o "<body>"
    o "<div id=\"vis\"></div>"

    drawThreads h events

    -- o "<ul>"
    -- forM_ events \ev -> do
    --   o $ "<li>" ++ show ev ++ "</li>"
    -- o "</ul>"

    -- let first_time = evTime (head events)
    -- let last_time = evTime (last events)
    -- let period_rel = last_time - first_time
    -- let period_rel_msec = period_rel `div` 1000
    -- let timeline_height = period_rel_msec

    -- o $ "<svg id=\"timeline\" width='700' height='" ++ show timeline_height ++ "'>"

    -- forM_ events \ev -> do
    --   let t      = evTime ev
    --   let t_rel  = t - first_time
    --   let t_msec = t_rel `div` 1000
    --   o $ "<text font-size='10' x=10 y=" ++ show t_msec ++ ">" ++ show ev ++ "</text>"
    --   pure t

    -- o "</svg>"

    o "</body>"
    o "</html>"

  putStrLn "Done"

type Cap = Int

data ThreadEvent
  = ThCreate Timestamp ThreadId
  | ThLabel  Timestamp ThreadId Text
  | ThStart  Timestamp ThreadId (Maybe Cap)
  | ThStop   Timestamp ThreadId ThreadStopStatus
  deriving (Show)

threadEventThreadId :: ThreadEvent -> ThreadId
threadEventThreadId = \case
  ThCreate _ tid   -> tid
  ThLabel  _ tid _ -> tid
  ThStart  _ tid _ -> tid
  ThStop   _ tid _ -> tid

threadEventTime :: ThreadEvent -> Timestamp
threadEventTime = \case
  ThCreate t _   -> t
  ThLabel  t _ _ -> t
  ThStart  t _ _ -> t
  ThStop   t _ _ -> t

threadEventsTimeRange :: [ThreadEvent] -> (Timestamp,Timestamp)
threadEventsTimeRange [] = error "No events"
threadEventsTimeRange (e:es) = go (threadEventTime e, threadEventTime e) es
  where
    go (!start,!end) = \case
      []     -> (start,end)
      (e:es) -> let t = threadEventTime e
                in go (min t start, max t end) es

data ThreadRange t
  = ThCreated t (Maybe Text)
  | ThRunning t t (Maybe Cap) ThreadStopStatus
  | ThPaused t t ThreadStopStatus
  deriving (Show,Functor)

filterThreadEvents :: [Event] -> [ThreadEvent]
filterThreadEvents events = mapMaybe is_thread_event events
  where
    is_thread_event ev = case evSpec ev of
      CreateThread tid  -> Just (ThCreate (evTime ev) tid)
      ThreadLabel tid l -> Just (ThLabel (evTime ev) tid l)
      RunThread tid     -> Just (ThStart (evTime ev) tid (evCap ev))
      StopThread tid s  -> Just (ThStop (evTime ev) tid s)
      _                 -> Nothing

makeThreadRanges :: [ThreadEvent] -> Map ThreadId [ThreadRange Timestamp]
makeThreadRanges th_events = th_ranges_by_tid
  where
    th_events_by_tid = foldr (\ev s -> Map.insertWith (++) (threadEventThreadId ev) [ev] s)
                             Map.empty
                             th_events

    make_thread_ranges mrun = \case
      [] -> case mrun of
        Nothing                          -> []
        Just (ThStop _ _ ThreadFinished) -> []
        Just e                           -> error $ "Thread not properly stopped: " ++ show e
      (e:es) -> case e of
        ThCreate t _ -> case es of
          -- label events are usually after CreateThread events
          (ThLabel _ _ l:es') -> ThCreated t (Just l) : make_thread_ranges Nothing es'
          _                   -> ThCreated t Nothing  : make_thread_ranges Nothing es

        -- We ignore label events that are not just after a CreateThread event
        ThLabel {} -> make_thread_ranges mrun es

        -- Generate ranges for blocked states
        ThStart tstart _ _
          | Just (ThStop tstop _ status) <- mrun
          , case status of
              ThreadFinished -> False -- don't generate ranges for finished threads
              _              -> True
          -> ThPaused tstop tstart status : make_thread_ranges (Just e) es

          | otherwise
          -> make_thread_ranges (Just e) es

        -- Generate ranges for running states
        ThStop tstop _ status
          | Just (ThStart tstart _ mcap) <- mrun
          -> ThRunning tstart tstop mcap status : make_thread_ranges (Just e) es

          | otherwise
          -> error $ "Stopped a thread that wasn't running: " ++ show e

    th_ranges_by_tid = Map.map (make_thread_ranges Nothing) th_events_by_tid



drawThreads :: Handle -> [Event] -> IO ()
drawThreads h events = do
  let o = hPutStrLn h

  let th_events = filterThreadEvents events
  let th_ranges = makeThreadRanges th_events
  let (mintime, maxtime) = threadEventsTimeRange th_events

  -- convert timestamps into coordinates
  let to_coord s = (s-mintime) `div` 1000 + 15
  let (mincoord,maxcoord) = (to_coord mintime, to_coord maxtime)
  let th_coords = Map.map (fmap (fmap to_coord)) th_ranges

  o $ "<svg id=\"timeline\" width='700' height='" ++ show (maxcoord + 50) ++ "'>"
  forM_ (zip (Map.toList th_coords) [0..]) \((tid,ranges),tnum) -> do
    let x = tnum * 30 + 15
    let line start stop sty = 
          o $ mconcat
            [ "<line x1=", show x
            , " y1=", show start
            , " x2=", show x
            , " y2=", show stop
            , " style=\"", sty, "\"/>"
            ]
    forM_ ranges \case
      ThCreated t mlbl                   -> do
        o $ mconcat
          [ "<circle cx=", show x
          , " cy=", show t
          , " r=3"
          , " stroke=\"orange\" stroke-width=\"3\" fill=\"orange\" />"
          ]
      ThRunning start stop _mcap status -> do
        line start stop "stroke:rgb(0,0,255);stroke-width:8"
        case status of
          ThreadFinished -> do
            o $ mconcat
              [ "<circle cx=", show x
              , " cy=", show stop
              , " r=3"
              , " stroke=\"black\" stroke-width=\"3\" fill=\"black\" />"
              ]
          _ -> pure ()
      ThPaused start stop status         -> case status of
        _ -> line start stop "stroke:rgb(255,0,0);stroke-width:8"

  o "</svg>"
