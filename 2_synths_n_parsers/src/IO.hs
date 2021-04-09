{-|
    Module      : IO
    Description : Derde checkpoint voor V2DeP: audio-synthesis en ringtone-parsing
    Copyright   : (c) Brian van der Bijl, 2020
    License     : BSD3
    Maintainer  : brian.vanderbijl@hu.nl

    In dit practicum gaan we audio genereren op basis van een abstracte representatie van noten.
    Daarnaast gaan we tekst in het RTTL formaat parsen tot deze abstracte representatie.

    Deze module bevat de IO functies die door beide executables gedeeld worden.
-}

module IO (playRTTL, playVLC, playFF, ps) where

import Types (Track, Beats, Instrument, getAsInts, (<+>))
import Parsers (parse)
import Instruments (sampleRate, generateWave, defaultInstrument)
import Util (fst3, uncurry3)

import System.Process (runCommand)
import Text.Printf (printf)
import Data.WAVE (putWAVEFile, WAVE(WAVE), WAVEHeader(WAVEHeader))

playRTTL :: Instrument -> String -> IO ()
playRTTL inst input = case parse input of
  Just (title, notes, bpm) -> playVLC [(title <> ".wav", bpm, [(inst, notes)])]
  Nothing -> putStrLn "Error"

playVLC :: [(FilePath, Beats, [Track])] -> IO ()
playVLC files = do
  mapM_ (uncurry3 waves) files
  _ <- runCommand . printf "vlc %s vlc://quit" . unwords . map fst3 $ files
  return ()

playFF :: FilePath -> Beats -> [Track] -> IO ()
playFF outputFilePath bpm tracks = do
  waves outputFilePath bpm tracks
  _ <- runCommand $ printf "ffplay -autoexit -showmode 1 -f f32le -ar %f %s" sampleRate outputFilePath
  return ()

waves :: FilePath -> Beats -> [Track] -> IO ()
waves filePath bpm = putWAVEFile filePath . WAVE (WAVEHeader 1 (round sampleRate) 32 Nothing) . map pure
                   . getAsInts . foldr1 (<+>) . map (\(i, ns) -> generateWave bpm i ns)

ps :: String -> IO ()
ps = playRTTL defaultInstrument
