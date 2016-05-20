module SpaceAge

open System

type Planet = 
    | Earth
    | Venus
    | Mars
    | Jupiter
    | Saturn
    | Uranus
    | Neptune
    | Mercury

let orbitalPeriodRelativeToEarthOn = 
    function
    | Earth -> 1m
    | Mercury -> 0.2408467m
    | Jupiter -> 11.862615m
    | Mars -> 1.8808158m
    | Neptune -> 164.79132m
    | Saturn -> 29.447498m
    | Uranus -> 84.016846m
    | Venus -> 0.61519726m

let secondsInAYearOn planet =
    let secondsInOneEarthYear = 31557600m
    secondsInOneEarthYear * orbitalPeriodRelativeToEarthOn planet

let round (number : decimal) = Math.Round(number, 2)

let spaceAge planet ageInSeconds = 
    ageInSeconds / (secondsInAYearOn planet) 
    |> round