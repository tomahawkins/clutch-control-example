module Main (main) where

import Language.ImProve

import Timer

main :: IO ()
main = do
  code C        "clutch_control" clutchControl
  code Ada      "clutch_control" clutchControl
  code Simulink "clutch_control" clutchControl
  code Modelica "clutch_control" clutchControl
  verify        "yices"          clutchControl

-- State variables.
engaged :: V Bool
engaged = global bool ["state", "engaged"] True

disengaging :: V Bool
disengaging = global bool ["state", "disengaging"] False

disengaged :: V Bool
disengaged = global bool ["state", "disengaged"] False

-- System inputs.
faultDetected :: E Bool
faultDetected = input bool ["inputs", "faultDetected"]

supplyValveClosed :: E Bool
supplyValveClosed = input bool ["inputs", "supplyValveClosed"]

-- A timer to cover supply valve closing.  Valve should close in 200 ms.
supplyValveTimer :: Timer
supplyValveTimer = mkTimer ["timers", "supplyValve"] 200

-- A timer to capture requirement of disengaging within 500 ms.
disengagementTimer :: Timer
disengagementTimer = mkTimer ["timers", "disengagementSpec"] 500

-- Clutch control FSM.
clutchControl :: Stmt ()
clutchControl = do
  case_ $ do

    ref engaged &&. faultDetected ==> do
      engaged     <== false
      disengaging <== true
      startTimer supplyValveTimer

    ref disengaging &&. (timerDone supplyValveTimer ||. supplyValveClosed) ==> do
      disengaging <== false
      disengaged  <== true

  updateTimer supplyValveTimer

  clutchControlSpecification

-- Clutch control specification.
clutchControlSpecification :: Stmt ()
clutchControlSpecification = do

  -- Start timer when it is time to disengage.
  if_ (faultDetected &&. not_ (timerActive disengagementTimer))
    (startTimer disengagementTimer)

  -- Only update the timer outside the disengaged state.
  if_ (not_ $ ref disengaged)
    (updateTimer disengagementTimer)

  assert "states_one_hot" 1 $      (ref engaged) &&. not_ (ref disengaging) &&. not_ (ref disengaged)
                          ||. not_ (ref engaged) &&.      (ref disengaging) &&. not_ (ref disengaged)
                          ||. not_ (ref engaged) &&. not_ (ref disengaging) &&.      (ref disengaged)

  assertTimerProperties supplyValveTimer 
  assertTimerProperties disengagementTimer

  assert "invariant" 1 $ ref engaged     &&. not_ (timerActive disengagementTimer)
                     ||. (   ref disengaging
                         &&. timerActive disengagementTimer
                         &&. timerActive supplyValveTimer
                         &&. timerValue disengagementTimer ==. timerValue supplyValveTimer
                         )
                     ||. ref disengaged

  -- Make sure the timer is never allowed to complete.
  assert "disengaged_within_500ms" 1 $ not_ $ timerDone disengagementTimer






