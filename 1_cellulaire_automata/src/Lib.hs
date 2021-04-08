{-|
    Module      : Lib
    Description : Tweede checkpoint voor V2DeP: cellulaire automata
    Copyright   : (c) Brian van der Bijl, 2020
    License     : BSD3
    Maintainer  : brian.vanderbijl@hu.nl

    In dit practicum gaan we aan de slag met 1D cellulaire automata [<https://mathworld.wolfram.com/Rule30.html>].
-}

module Lib where

import Data.Maybe (catMaybes) -- Niet gebruikt, maar deze kan van pas komen...
import Data.List (unfoldr)
import Data.Tuple (swap)

-- Om de state van een cellulair automaton bij te houden bouwen we eerst een set functies rond een `FocusList` type. Dit type representeert een 1-dimensionale lijst, met een
-- enkel element dat "in focus" is. Het is hierdoor mogelijk snel en makkelijk een enkele cel en de cellen eromheen te bereiken.

-- * FocusList

{- | The focussed list [0 1 2 ⟨3⟩ 4 5] is represented as @FocusList [3,4,5] [2,1,0]@. The first element (head) of the first list is focussed, and is easily and cheaply accessible.
 -   The items before the focus are placed in the backwards list in reverse order, so that we can easily move the focus by removing the focus-element from one list and prepending
 -   it to the other.
-}
data FocusList a = FocusList { forward :: [a]
                             , backward :: [a]
                             }
  deriving Show

-- De instance-declaraties mag je voor nu negeren.
instance Functor FocusList where
  fmap = mapFocusList

-- Enkele voorbeelden om je functies mee te testen:
intVoorbeeld :: FocusList Int
intVoorbeeld = FocusList [3,4,5] [2,1,0]

stringVoorbeeld :: FocusList String
stringVoorbeeld = FocusList ["3","4","5"] ["2","1","0"]

-- TODO Schrijf en documenteer een functie die een focus-list omzet in een gewone lijst. Het resultaat bevat geen focus-informatie meer, maar moet wel op de juiste volgorde staan.
-- toList intVoorbeeld ~> [0,1,2,3,4,5]
toList :: FocusList a -> [a]
addTo list [] = list
-- ^ Voegt de lijsten samen.
addTo addList (x:xs) = addTo (x : addList) xs

toList (FocusList forward backward) =  addTo forward backward

-- TODO Schrijf en documenteer een functie die een gewone lijst omzet in een focus-list. Omdat een gewone lijst geen focus heeft moeten we deze kiezen; dit is altijd het eerste element.
fromList :: [a] -> FocusList a
-- ^ Voeg de head toe aan een apparte lijst. In de focuslist is het eerste element nu de focus.
fromList list = FocusList list []

-- | Move the focus one to the left
goLeft :: FocusList a -> FocusList a
-- ^ Voegt de head toe van foward aan backward (fw, bw)
goLeft (FocusList fw (f:bw)) = FocusList (f:fw) bw

-- TODO Schrijf en documenteer zelf een functie goRight die de focuslist een plaats naar rechts opschuift.

goRight :: FocusList a -> FocusList a
-- ^ Voegt de head toe van foward toe aan backward (fw, bw)
goRight (FocusList(b: fw) bw)= FocusList fw (b:bw)

-- TODO Schrijf en documenteer een functie leftMost die de focus geheel naar links opschuift.
leftMost :: FocusList a -> FocusList a
addRec list [] = list
-- ^ Voegt recursief de elementen uit backward toe aan forward totdat hij leeg is.
addRec l1(x:xs) = addRec (x:l1) xs

leftMost (FocusList fw bw) = FocusList [] (addRec bw fw)

-- TODO Schrijf en documenteer een functie rightMost die de focus geheel naar rechts opschuift.
rightMost :: FocusList a -> FocusList a
-- Doet het zelfde als leftMost maar dan omgedraaid waardoor de elementen uit foward bij backward worden toegevoegd.
rightMost (FocusList fw bw) = FocusList [] (addRec fw bw)

-- De functies goLeft en goRight gaan er impliciet vanuit dat er links respectievelijk rechts een cell gedefinieerd is. De aanroep `goLeft $ fromList [1,2,3]` zal echter crashen
-- omdat er in een lege lijst gezocht wordt: er is niets links. Dit is voor onze toepassing niet handig, omdat we bijvoorbeeld ook de context links van het eerste vakje nodig
-- hebben om de nieuwe waarde van dat vakje te bepalen (en dito voor het laatste vakje rechts).

-- TODO Schrijf en documenteer de functies totalLeft en totalRight die de focus naar links respectievelijk rechts opschuift; als er links/rechts geen vakje meer is, dan wordt een
-- lege (dode) cel teruggeven. Hiervoor gebruik je de waarde `mempty`, waar we met een later college nog op in zullen gaan. Kort gezegd zorgt dit ervoor dat de FocusList ook
-- op andere types blijft werken - je kan dit testen door totalLeft/totalRight herhaaldelijk op de `voorbeeldString` aan te roepen, waar een leeg vakje een lege string zal zijn.

-- [⟨░⟩, ▓, ▓, ▓, ▓, ░]  ⤚goLeft→ [⟨░⟩, ░, ▓, ▓, ▓, ▓, ░]

totalLeft :: (Eq a, Monoid a) => FocusList a -> FocusList a
-- ^ Geeft mempty mee aan foward wanneer backwards leeg is.
totalLeft (FocusList [] a) = FocusList [] (mempty : a)
-- ^ De goLeft functie
totalLeft (FocusList fw (f:bw)) = FocusList (f:fw) bw

totalRight :: (Eq a, Monoid a) => FocusList a -> FocusList a
-- ^ Geeft mempty mee aan backward wanneer foward leeg is.
totalRight (FocusList [] a) = FocusList [] (mempty : a)
-- ^ De goRight functie
totalRight (FocusList (b:fw) bw) = FocusList fw (b:bw)

-- TODO In de colleges hebben we kennis gemaakt met een aantal hogere-orde functies zoals `map`, `zipWith` en `fold[r/l]`. Hier zullen we equivalenten voor de FocusList opstellen.
-- De functies mapFocusList werkt zoals je zou verwachten: de functie wordt op ieder element toegepast, voor, op en na de focus. Je mag hier gewoon map voor gebruiken

mapFocusList :: (a -> b) -> FocusList a -> FocusList b
-- ^
mapFocusList f fl = fromList ((map f (toList fl)))

-- TODO De functie zipFocusList zorgt ervoor dat ieder paar elementen uit de FocusLists als volgt met elkaar gecombineerd wordt:
-- [1, 2, ⟨3⟩,  4, 5]
-- [  -1, ⟨1⟩, -1, 1, -1]
--------------------------- (*)
-- [  -2, ⟨3⟩, -4, 5    ]

-- Oftewel: de megegeven functie wordt aangeroepen op de twee focus-elementen, met als resultaat het nieuwe focus-element. Daarnaast wordt de functie paarsgewijs naar
-- links/rechts doorgevoerd, waarbij gestopt wordt zodra een van beide uiteinden leeg is. Dit laatste is net als bij de gewone zipWith, die je hier ook voor mag gebruiken.

zipFocusListWith :: (a -> b -> c) -> FocusList a -> FocusList b -> FocusList c
-- ^
zipFocusListWith f (FocusList x xs) (FocusList y ys) = FocusList(zipWith(f)(x)(y)) (zipWith (f)(xs)(ys))

-- TODO Het folden van een FocusList vergt de meeste toelichting: waar we met een normale lijst met een left fold en een right fold te maken hebben, moeten we hier vanuit de focus werken.
-- Vanuit de focus worden de elementen van rechts steeds gecombineerd tot een nieuw element, vanuit het element voor de focus gebeurt hetzelfde vanuit links. De twee resultaten van
-- beide sublijsten (begin tot aan focus, focus tot en met eind) worden vervolgens nog een keer met de meegegeven functie gecombineerd. Hieronder een paar voorbeelden:

-- foldFocusList (*) [0, 1, 2, ⟨3⟩, 4, 5] = (0 * (1 * 2)) * ((3 * 4) * 5)

-- foldFocusList (-) [0, 1, 2, ⟨3⟩, 4, 5] = (0 - (1 - 2)) - ((3 - 4) - 5)
-- foldFocusList (-) [0, 1, 2, ⟨3⟩, 4, 5] = (0 - (-1)) - ((-1) - 5)
-- foldFocusList (-) [0, 1, 2, ⟨3⟩, 4, 5] = 1 - (-6)
-- foldFocusList (-) [0, 1, 2, ⟨3⟩, 4, 5] = 7

-- Je kunt `testFold` gebruiken om je functie te testen. Denk eraan dat de backwards lijst achterstevoren staat, en waarschijnlijk omgekeerd moet worden.

foldFocusList :: (a -> a -> a) -> FocusList a -> a
-- ^
foldFocusList f (FocusList fw bw) =  (foldr1 f (reverse bw)) `f` (foldl1 f fw)

-- | Test function for the behaviour of foldFocusList.
testFold :: Bool
testFold = and [ foldFocusList (+) intVoorbeeld     == 15
               , foldFocusList (-) intVoorbeeld     == 7
               , foldFocusList (++) stringVoorbeeld == "012345"
               ]

-- * Cells and Automata

-- Nu we een redelijk complete FocusList hebben kunnen we gaan kijken naar daadwerkelijke celulaire automata, te beginnen met de Cell.

-- | A cell can be either on or off, dead or alive. What basic type could we have used instead? Why would we choose to roll our own equivalent datatype?
data Cell = Alive | Dead deriving (Show, Eq)

-- De instance-declaraties mag je voor nu negeren.
instance Semigroup Cell where
  Dead <> x = x
  Alive <> x = Alive

instance Monoid Cell where
  mempty = Dead

-- | The state of our cellular automaton is represented as a FocusList of Cells.
type Automaton = FocusList Cell

-- | Start state, per default, is a single live cell.
start :: Automaton
start = FocusList [Alive] []

-- | Alternative start state with 5 alive cells, for shrinking rules.
fiveAlive :: Automaton
fiveAlive = fromList $ replicate 5 Alive

-- | A rule [<https://mathworld.wolfram.com/Rule30.html>] is a mapping from each possible combination of three adjacent cells to the associated "next state".
type Context = [Cell]
type Rule = Context -> Cell

-- * Rule Iteration

-- TODO Schrijf en documenteer een functie safeHead die het eerste item van een lijst geeft; als de lijst leeg is wordt een meegegeven default values teruggegeven.
safeHead :: a        -- ^ Default value
         -> [a]      -- ^ Source list
         -> a
safeHead b [] = b
safeHead b (x:xs) = x

-- TODO Schrijf en documenteer een functie takeAtLeast die werkt als `take`, maar met een extra argument. Als de lijst lang genoeg is, bijvoorbeeld
-- `takeAtLeast 3 "0" ["1","2","3","4","5"]` dan werkt de functie hetzelfde als `take` en worden de eerste `n` (hier 3) elementen teruggegeven.
-- Als dat niet zo is dan worden zoveel mogelijk elementen teruggegeven, en wordt de lijst daarna tot de gevraagde lengte aangevuld met een
-- meegegeven default-waarde: `takeAtLeast 3 "0" ["1"] ~> ["1", "0", "0"]`.
takeAtLeast :: Int   -- ^ Number of items to take
            -> a     -- ^ Default value added to the right as padding
            -> [a]   -- ^ Source list
            -> [a]
takeAtLeast n d list
    | n <=length list = take n list
    | n > (length list) = list ++ replicate(n- length list) d

-- TODO Schrijf en documenteer een functie context die met behulp van takeAtLeast de context van de focus-cel in een Automaton teruggeeft. Niet-gedefinieerde cellen zijn per definitie Dead.
context :: Automaton -> Context
context (FocusList fw bw) = (takeAtLeast 1 Dead bw) ++ (takeAtLeast 2 Dead fw)

-- TODO Schrijf en documenteer een functie expand die een Automaton uitbreid met een dode cel aan beide uiteindes. We doen voor deze simulatie de aanname dat de "known universe"
-- iedere ronde met 1 uitbreid naar zowel links als rechts.
expand :: Automaton -> Automaton
expand (FocusList fw bw) = (FocusList (fw ++ [Dead]) (bw ++ [Dead]))

-- | A sequence of Automaton-states over time is called a TimeSeries.
type TimeSeries = [Automaton]

-- TODO Voorzie onderstaande functie van interne documentatie, d.w.z. zoek uit en beschrijf hoe de recursie verloopt. Zou deze functie makkelijk te schrijven zijn met behulp van
-- de hogere-orde functies die we in de les hebben gezien? Waarom wel/niet?

-- | Iterate a given rule @n@ times, given a start state. The result will be a sequence of states from start to @n@.
iterateRule :: Rule          -- ^ The rule to apply
            -> Int           -- ^ How many times to apply the rule
            -> Automaton     -- ^ The initial state
            -> TimeSeries
iterateRule r 0 s = [s]
iterateRule r n s = s : iterateRule r (pred n) (fromList $ applyRule $ leftMost $ expand s)
  where applyRule :: Automaton -> Context
        applyRule (FocusList [] bw) = []
        applyRule z = r (context z) : applyRule (goRight z)

-- | Convert a time-series of Automaton-states to a printable string.
showPyramid :: TimeSeries -> String
showPyramid zs = unlines $ zipWith showFocusList zs $ reverse [0..div (pred w) 2]
  where w = length $ toList $ last zs :: Int
        showFocusList :: Automaton -> Int -> String
        showFocusList z p = replicate p ' ' <> concatMap showCell (toList z)
        showCell :: Cell -> String
        showCell Dead  = "░"
        showCell Alive = "▓"

-- TODO Vul de functie rule30 aan met de andere 7 gevallen. Je mag de voorbeeldregel aanpassen/verwijderen om dit in minder regels code te doen. De underscore _ is je vriend.
rule30 :: Rule
rule30 [Dead, Dead, Dead] = Dead
-- ...

-- Je kan je rule-30 functie in GHCi testen met het volgende commando:
-- putStrLn . showPyramid . iterateRule rule30 15 $ start

-- De verwachte uitvoer is dan:
{-             ▓
              ▓▓▓
             ▓▓░░▓
            ▓▓░▓▓▓▓
           ▓▓░░▓░░░▓
          ▓▓░▓▓▓▓░▓▓▓
         ▓▓░░▓░░░░▓░░▓
        ▓▓░▓▓▓▓░░▓▓▓▓▓▓
       ▓▓░░▓░░░▓▓▓░░░░░▓
      ▓▓░▓▓▓▓░▓▓░░▓░░░▓▓▓
     ▓▓░░▓░░░░▓░▓▓▓▓░▓▓░░▓
    ▓▓░▓▓▓▓░░▓▓░▓░░░░▓░▓▓▓▓
   ▓▓░░▓░░░▓▓▓░░▓▓░░▓▓░▓░░░▓
  ▓▓░▓▓▓▓░▓▓░░▓▓▓░▓▓▓░░▓▓░▓▓▓
 ▓▓░░▓░░░░▓░▓▓▓░░░▓░░▓▓▓░░▓░░▓
▓▓░▓▓▓▓░░▓▓░▓░░▓░▓▓▓▓▓░░▓▓▓▓▓▓▓ -}

-- * Rule Generation

-- Er bestaan 256 regels, die we niet allemaal met de hand gaan uitprogrammeren op bovenstaande manier. Zoals op de genoemde pagina te zien is heeft het nummer te maken met binaire
-- codering. De toestand van een cel hangt af van de toestand van 3 cellen in de vorige ronde: de cel zelf en diens beide buren (de context). Er zijn 8 mogelijke combinaties
-- van 3 van dit soort cellen. Afhankelijke van het nummer dat een regel heeft mapt iedere combinatie naar een levende of dode cel.

-- TODO Definieer allereerst een constante `inputs` die alle 8 mogelijke contexts weergeeft: [Alive,Alive,Alive], [Alive,Alive,Dead], etc.
-- Je mag dit met de hand uitschrijven, maar voor extra punten kun je ook een lijst-comprehensie of andere slimme functie verzinnen.

inputs :: [Context]
inputs = [[a, b, c] | a <- [Alive, Dead],
                      b <- [Alive, Dead],
                      c <- [Alive, Dead] ]

-- | If the given predicate applies to the given value, return Just the given value; in all other cases, return Nothing.
guard :: (a -> Bool) -> a -> Maybe a
guard p x | p x = Just x
          | otherwise = Nothing

-- TODO Deze functie converteert een Int-getal naar een binaire representatie [Bool]. Zoek de definitie van `unfoldr` op met Hoogle en `guard` in Utility.hs; `toEnum` converteert
-- een Int naar een ander type, in dit geval 0->False en 1->True voor Bool. Met deze kennis, probeer te achterhalen hoe de binary-functie werkt en documenteer dit met Haddock.
binary :: Int -> [Bool]
binary = map toEnum . reverse . take 8 . (++ repeat 0)
       . unfoldr (guard (/= (0,0)) . swap . flip divMod 2)

-- TODO Schrijf en documenteer een functie mask die, gegeven een lijst Booleans en een lijst elementen alleen de elementen laat staan die (qua positie) overeenkomen met een True.
-- Je kan hiervoor zipWith en Maybe gebruiken (check `catMaybes` in Data.Maybe) of de recursie met de hand uitvoeren.
mask :: [Bool] -> [a] -> [a]
mask bList eList = catMaybes $ zipWith (\b e -> if b then Just e else Nothing) bList eList

-- TODO Combineer `mask` en `binary` met de library functie `elem` en de eerder geschreven `inputs` tot een rule functie. Denk eraan dat het type Rule een short-hand is voor een
-- functie-type, dus dat je met 2 argumenten te maken hebt. De Int staat hierbij voor het nummer van de regel, dat je eerst naar binair moet omrekenen; de Context `input` is
-- waarnaar je kijkt om te zien of het resultaat met de gevraagde regel Dead or Alive is. Definieer met `where` subset van `inputs` die tot een levende danwel dode cel leiden.
-- Vergeet niet je functie te documenteren.
rule :: Int -> Rule
rule n input = if elem (mask (binary n) input) inputs
                then True
                else False

{- Je kan je rule-functie in GHCi testen met variaties op het volgende commando:

   putStrLn . showPyramid . iterateRule (rule 18) 15 $ start

                  ▓
                 ▓░▓
                ▓░░░▓
               ▓░▓░▓░▓
              ▓░░░░░░░▓
             ▓░▓░░░░░▓░▓
            ▓░░░▓░░░▓░░░▓
           ▓░▓░▓░▓░▓░▓░▓░▓
          ▓░░░░░░░░░░░░░░░▓
         ▓░▓░░░░░░░░░░░░░▓░▓
        ▓░░░▓░░░░░░░░░░░▓░░░▓
       ▓░▓░▓░▓░░░░░░░░░▓░▓░▓░▓
      ▓░░░░░░░▓░░░░░░░▓░░░░░░░▓
     ▓░▓░░░░░▓░▓░░░░░▓░▓░░░░░▓░▓
    ▓░░░▓░░░▓░░░▓░░░▓░░░▓░░░▓░░░▓
   ▓░▓░▓░▓░▓░▓░▓░▓░▓░▓░▓░▓░▓░▓░▓░▓

   putStrLn . showPyramid . iterateRule (rule 128) 10 $ fiveAlive

               ▓▓▓▓▓
              ░░▓▓▓░░
             ░░░░▓░░░░
            ░░░░░░░░░░░
           ░░░░░░░░░░░░░
          ░░░░░░░░░░░░░░░
         ░░░░░░░░░░░░░░░░░
        ░░░░░░░░░░░░░░░░░░░
       ░░░░░░░░░░░░░░░░░░░░░
      ░░░░░░░░░░░░░░░░░░░░░░░
     ░░░░░░░░░░░░░░░░░░░░░░░░░

   Als het goed is zal `stack run` nu ook werken met de voorgeschreven main functie; experimenteer met verschillende parameters en zie of dit werkt.
-}
