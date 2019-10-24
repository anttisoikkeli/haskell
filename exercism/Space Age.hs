module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

ageOn :: Planet -> Float -> Float
ageOn planet seconds =
  case planet of
    Mercury -> y / 0.2408467
    Venus -> y / 0.61519726
    Earth -> y
    Mars -> y / 1.8808158
    Jupiter -> y / 11.862615
    Saturn -> y / 29.447498
    Uranus -> y / 84.016846
    Neptune -> y / 164.79132
  where y = seconds / 31557600
