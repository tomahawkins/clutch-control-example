module Timer
  ( Timer
  , mkTimer
  , updateTimer
  , startTimer
  , deactivateTimer
  , timerActive
  , timerDone
  , timerValue
  , assertTimerProperties
  ) where

import Data.List
import Language.ImProve

-- | A timer object.
data Timer = Timer [Name] Int (V Bool) (V Int)

-- | Creates a new timer given a path and a time.
mkTimer :: [Name] -> Int -> Timer
mkTimer path time
  | time <= 0 = error "mkTimer: time must be > 0."
  | otherwise = Timer path time (global bool (path ++ ["active"]) False) (global int (path ++ ["count"]) 0)

-- | Updates a timer.
updateTimer :: Timer -> Stmt ()
updateTimer (Timer _ time active count) = do
  if_ (ref active &&. ref count <. constant time) (incr count)

-- | Starts a timer.
startTimer :: Timer -> Stmt ()
startTimer (Timer _ _ active count) = do
  active <== true
  count  <== 0

-- | Deactivate a timer.
deactivateTimer :: Timer -> Stmt ()
deactivateTimer (Timer _ _ active count) = do
  active <== false
  count  <== 0

-- | Checks if a timer is active.
timerActive :: Timer -> E Bool
timerActive (Timer _ _ active _) = ref active

-- | Checks if a timer is done.  Returns false if timer is not active.
timerDone :: Timer -> E Bool
timerDone t@(Timer _ time _ count) = timerActive t &&. ref count ==. constant time

-- | Returns the current timer value.
timerValue :: Timer -> E Int
timerValue (Timer _ _ _ count) = ref count

-- | Asserts basic timer properties such as timer range and inactive at 0.
assertTimerProperties :: Timer -> Stmt ()
assertTimerProperties (Timer path time active count) = assert (intercalate "." $ path ++ ["properties"]) 1 $ ref count >=. 0 &&. ref count <=. constant time &&. (not_ (ref active) --> ref count ==. 0)

