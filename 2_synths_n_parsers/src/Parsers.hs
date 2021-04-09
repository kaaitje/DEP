{-|
    Module      : Parsers
    Description : Derde checkpoint voor V2DeP: audio-synthesis en ringtone-parsing
    Copyright   : (c) Brian van der Bijl, 2020
    License     : BSD3
    Maintainer  : brian.vanderbijl@hu.nl

    In dit practicum gaan we audio genereren op basis van een abstracte representatie van noten.
    Daarnaast gaan we tekst in het RTTL formaat parsen tot deze abstracte representatie.

    Deze module bevat parsers en functies om deze te combineren. We gebruiken simpele parsers om meer complexe parsers op te bouwen.
-}

module Parsers (Parser, parse, pCharSet, pComplementCharSet, pString, pOptional, pRepeat, pNumber, pOctave, pHeader, parse) where

import Types (Octave, Beats, Duration(..), Note(..), Tone(..))
 
import Control.Monad.Trans.State (StateT(..), evalStateT, put, get)
import Data.Maybe (isJust, fromMaybe)
import Data.Char (toUpper)
import Control.Monad(mzero, mplus)
import Data.List (uncons)

type Parser = (StateT String Maybe)
type CharSet = [Char]

pCharSet :: CharSet -> Parser Char
pCharSet cs = do input <- uncons <$> get
                 case input of
                   Just (h, t) -> if h `elem` cs then put t >> return h else mzero
                   Nothing -> mzero

-- TODO Schrijf een `Parser` `pComplementCharSet` die een lijst van karakters meekrijgt (ook wel een `String`) en het eerste karakter parset wanneer dit niet in de meegegeven set karakters zit.
pComplementCharSet :: CharSet -> Parser Char
pComplementCharSet cs = do input <- uncons <$> get
                           case input of
                             Just (h, t) -> if h `elem` cs then mzero else put t >> return h
                             Nothing -> mzero

-- TODO Schrijf een `Parser` `pString` die een gegegeven `String` probeert te parsen. Gebruik hiervoor `do` notatie; parse een enkele letter met `pCharSet` en parse de rest recursief met `pString`; combineer beide waarden weer voordat je deze `return`t. Vergeet niet een geval voor een lege `String`  te schrijven.
pString :: String -> Parser String
{- |
Parset eerste char uit de lijst en stopt deze in firstChar
Parset daarna recursief de rest (tail) en zet de Chars in 'rest'
Koppel firstchar en rest
Pattern match wanneer de lijst helemaal leeg is, return dan mzero
-}
pString (x:xs) = do firstChar <- pCharSet [x]
                    rest <- pString xs
                    return $ firstChar : rest
pString []     = return mzero

-- TODO Schrijf een `Parser` `pOpttional` die gegeven een `Parser` optioneel maakt. Als de `Parser` slaagt wordt een `Just` value teruggegeven, zo niet wordt `Nothing` ge`return`ed. Je kunt hiervoor gebruik maken van `mplus` uit `Control.Monad`.
pOptional :: Parser a -> Parser (Maybe a)
{- |
voert de parser uit met behulp van fmap
-}
pOptional p = fmap Just p <|> return Nothing

pRepeatSepBy :: Parser a -> Parser b -> Parser [b]
pRepeatSepBy sep p = (:) <$> p <*> mplus (sep *> pRepeatSepBy sep p) (return [])

-- De empty parser, deze parset niets en geeft `()` terug.
pEmpty :: Parser ()
pEmpty = return ()

-- TODO Combineer `pRepeatSepBy` en `pEmpty` tot een `Parser` `pRepeat` die een enkele `Parser` herhaalt.
pRepeat :: Parser a -> Parser [a]
pRepeat p = pRepeatSepBy pEmpty p

numbers :: CharSet
numbers = "0123456789"

-- TODO Combineer `pRepeat` en `pCharSet` tot een `Parser` die een getal als `String` leest, en roep hier `read` op aan om een `Int` terug te geven.
pNumber :: Parser Int
{- |
Parse over numbers als string.
voer read uit met fmap en met output van de parse
-}
pNumber = fmap (read) (pRepeat $ pCharSet numbers)

pTone :: Parser Tone
pTone = do tone <- tRead . toUpper <$> pCharSet "abcdefg"
           sharp <- pOptional (pCharSet "#")
           if isJust sharp && tone `elem` [C,D,F,G,A]
             then return (succ tone)
             else return tone
  where tRead 'C' = C
        tRead 'D' = D
        tRead 'E' = E
        tRead 'F' = F
        tRead 'G' = G
        tRead 'A' = A
        tRead 'B' = B
        tRead _   = error "Invalid note"

-- TODO Schrijf een `Parser` `pOctave`. Je kunt `toEnum` gebruiken om een `Int` naar een `Octave` te casten.
pOctave :: Parser Octave
{- |
past toEnum toe op pNumber
-}
pOctave = toEnum <$> pNumber

pDuration :: Parser Duration
pDuration = do number <- pNumber
               case number of
                 1 -> return Full
                 2 -> return Half
                 4 -> return Quarter
                 8 -> return Eighth
                 16 -> return Sixteenth
                 32 -> return Thirtysecond
                 _ -> mzero

pPause :: Duration -> Parser Note
pPause d = do duration <- fromMaybe d <$> pOptional pDuration
              _ <- pCharSet "pP"
              return $ Pause duration

pNote :: Duration -> Octave -> Parser Note
pNote d o = do duration <- fromMaybe d <$> pOptional pDuration
               tone <- pTone
               dot <- pOptional (pCharSet ".")
               octave <- fromMaybe o <$> pOptional pOctave
               return $ Note tone octave (if isJust dot then Dotted duration else duration)

pComma :: Parser ()
pComma = () <$ do _ <- pCharSet ","
                  pOptional (pCharSet " ")

-- TODO Pas deze `Parser` aan zodat de de titel uit de RTTL string wordt gehaald en in de plaats van PLACEHOLDER wordt teruggegeven.
pHeader :: Parser (String, Duration, Octave, Beats)
pHeader = do _ <- pRepeat (pComplementCharSet ":")
             _ <- pCharSet ":"
             _ <- pOptional (pCharSet " ")
             _ <- pString "d="
             duration <- pDuration
             _ <- pComma
             _ <- pString "o="
             octave <- pOctave
             _ <- pComma
             _ <- pString "b="
             bpm <- fromIntegral <$> pNumber
             _ <- pCharSet ":"
             _ <- pOptional (pCharSet " ")
             return ("PLACEHOLDER", duration, octave, bpm)

pSeparator :: Parser ()
pSeparator = () <$ foldl1 mplus [pString " ", pString ", ", pString ","]

pRTTL :: Parser (String, [Note], Beats)
pRTTL = do (t, d, o, b) <- pHeader
           notes <- pRepeatSepBy pSeparator $ mplus (pNote d o) (pPause d)
           return (t, notes, b)

-- TODO Schrijf een functie `parse` die `pRTTL` aanroept. Bedenk hierbij dat een `Parser` eigenlijk niet meer is dan een `StateT` met een `Maybe` erin. 
parse :: String -> Maybe (String, [Note], Beats)
{- |
Geeft string mee aan pRTTL.
-}
parse str = evalStateT pRTTL str
