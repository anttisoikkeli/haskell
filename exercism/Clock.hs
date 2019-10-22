module Clock (addDelta, fromHourMin, toString) where

data Clock = Clock { hour :: Int, minute :: Int } deriving (Show, Eq)

fix :: Int -> Int -> Int
fix v target = if v < 0 then fix (v + target) target else v

proShow :: Int -> String
proShow v = if v < 10 then "0" ++ show v else show v

fromHourMin :: Int -> Int -> Clock
fromHourMin hourAdd minuteAdd = addDelta hourAdd minuteAdd (Clock 0 0)

toString :: Clock -> String
toString clock = (proShow . hour $ clock) ++ ":" ++ (proShow . minute $ clock)

addDelta :: Int -> Int -> Clock -> Clock
addDelta hourAdd minuteAdd clock =
  let fixedMinutes = fix minuteAdd (24*60)
      newHour = rem (hour clock + fix hourAdd 24 + quot (minute clock + fixedMinutes) 60) 24
      newMinute = rem (minute clock + fixedMinutes) 60
   in Clock newHour newMinute
