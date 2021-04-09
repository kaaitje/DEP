{-|
    Module      : Types
    Description : Derde checkpoint voor V2DeP: audio-synthesis en ringtone-parsing
    Copyright   : (c) Brian van der Bijl, 2020
    License     : BSD3
    Maintainer  : brian.vanderbijl@hu.nl

    In dit practicum gaan we audio genereren op basis van een abstracte representatie van noten.
    Daarnaast gaan we tekst in het RTTL formaat parsen tot deze abstracte representatie.

    Deze module bevat alle type-declaraties, instanties en type-gerelateerde functies, zodat we deze makkelijk in meerdere modules kunnen importeren.
-}

{-# LANGUAGE TypeApplications #-}

module Types ( Beats, Hz, Samples, Seconds, Semitones, Track, Ringtone
             , Tone(..), Octave(..), Duration(..), Note(..)
             , Sound, floatSound, intSound, (<+>), asFloatSound, asIntSound, getAsFloats, getAsInts
             , Instrument, instrument, Modifier, modifier, modifyInstrument, arrange
             ) where

import Data.Int (Int32)

type Pulse = [Float]
type Seconds = Float
type Samples = Float
type Hz = Float
type Semitones = Float
type Beats = Float
type Ringtone = String

data Tone = C | CSharp | D | DSharp | E | F | FSharp | G | GSharp | A | ASharp | B deriving (Enum, Eq, Show)
data Octave = Zero | One | Two | Three | Four | Five | Six | Seven | Eight deriving (Enum, Eq, Show)
data Duration = Full | Half | Quarter | Eighth | Sixteenth | Thirtysecond | Dotted Duration deriving (Eq, Show)
data Note = Pause Duration | Note Tone Octave Duration deriving (Eq, Show)

data Sound = IntFrames [Int32] | FloatFrames [Float]
  deriving Show

floatSound :: [Float] -> Sound
floatSound = FloatFrames

intSound :: [Int32] -> Sound
intSound = IntFrames

instance Eq Sound where
  (FloatFrames xs) == (FloatFrames ys) = all ((<  0.001) . abs) $ zipWith (-) xs ys
  (IntFrames xs) == (IntFrames ys)     = all ((< 10    ) . abs) $ zipWith (-) xs ys
  _ == _                               = False

-- TODO Maak instances voor `Sound` voor `Semigroup` en `Monoid`. De monoid-operatie staat in dit geval voor het sequentieel (achter elkaar) combineren van audiofragmenten. Het is van belang dat `IntFrames` alleen met `IntFrames` worden gecombineerd, en dito voor `FloatFrames`. Bij twee verschillende gevallen moet je beiden naar hetzelfde formaat converteren, idealiter `FloatFrames`. Wat is een leeg audiofragment in deze context?

instance Semigroup Sound where
  {- |
  Floatframen en IntFrames kan je koppelen
  IntFrames moet eerst veranderd worden naar floats voordat je ze kan samenvoegen
  -}
  (IntFrames a) <> (IntFrames b) = IntFrames (a ++ b)
  (FloatFrames a) <> (FloatFrames b) = FloatFrames (a ++ b)
  (FloatFrames a) <> (IntFrames b) = FloatFrames (a ++ getAsFloats (intSound b))
  (IntFrames a) <> (FloatFrames b) = FloatFrames (getAsFloats (intSound a) ++ b)

instance Monoid Sound wherex
  mempty = IntFrames[]

-- TODO Maak een operator `(<+>)` die twee `Sound`s  tot een enkel `Sound` combineert door de geluiden tegelijk af te spreken. Dit mag door de frames als in een `zipWith (+)` samen te voegen, maar in dit geval wordt niet de kortste maar de langste lijst aangehouden (in dat geval wordt enkel het aanwezige geluid weergegeven). Je hoeft deze operator alleen op `FloatFrames` te matchen, de laatste regel converteert alles hierheen als een of beide argumenten in `IntFrames` staan.

(<+>) :: Sound -> Sound -> Sound
(FloatFrames x) <+> (FloatFrames y)
  | length x >= length y = FloatFrames (zipWithL (+) x y)
  | length x < length y = FloatFrames (zipWithR (+) x y)
x <+> y = asFloatSound x <+> asFloatSound y

asFloatSound :: Sound -> Sound
asFloatSound (IntFrames fs) = floatSound $ map ( (/ fromIntegral (div (maxBound @Int32 ) 2 )) . fromIntegral ) fs
asFloatSound fframe = fframe

-- TODO Maak een functie `asIntSOund` die als inverse van `asFloatSound` fungeert.
asIntSound :: Sound -> Sound
{- |
Dit moet het tegenovergestelde zijn van asFloatSound.
vermenigvuldig elk getal in de lijst met int32.
Daarna rond hij elk getal af.
Als laatst maakt hij er weer een van.
-}
asIntSound (FloatFrames fs) = intSound $ map (\x -> round (fromIntegral ( maxBound @Int32 `div` 2 ) * x)) fs
asIntSound fframe = fframe

getAsFloats :: Sound -> [Float]
getAsFloats sound = case asFloatSound sound of
  (FloatFrames ffs) -> ffs
  _ -> error "asFloatSound did not return FloatFrames"

getAsInts :: Sound -> [Int32]
getAsInts sound = case asIntSound sound of
  (IntFrames ifs) -> ifs
  _ -> error "asIntSound did not return IntFrames"

type Track = (Instrument, [Note])

newtype Instrument = Instrument (Hz -> Seconds -> Pulse)

instrument :: (Hz -> Seconds -> Pulse) -> Instrument
instrument = Instrument

newtype Modifier = Modifier (Pulse -> Pulse)

modifier :: (Pulse -> Pulse) -> Modifier
modifier = Modifier

instance Semigroup Modifier where
  (Modifier m1) <> (Modifier m2) = Modifier $ m1 . m2

-- TODO Maak een functie `modifyInstrument) die een `Modifier` met een `Instrument` combineert. Gebruik een lambda om een nieuw instrument terug te geven, waarbij de functie in de modifier met de functie in het instrument gecomposed wordt.

modifyInstrument :: Instrument -> Modifier -> Instrument
modifyInstrument (Instrument inst) (Modifier mod) = Instrument (\a b -> mod $ inst a b)

-- TODO Maak een functie `arrange` die de functie in het meegegeven `Instrument` toepast op de frequentie en duur. Het resultaat wordt als `Sound` verpakt.
arrange :: Instrument -> Hz -> Seconds -> Sound
{- |
Gebruikt intrument
Maakt een nieuw instrument en maakt er een Sound van.
Een instrument geeft een Pulse, een float.
Pulse wordt meegegeven aan floatSound
-}
arrange (Instrument inst) h s = floatSound (inst h s)