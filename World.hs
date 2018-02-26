{-
Dan Noar
David Rubio Vallejo

This implements a Computational Tree Logic (CTL) Branching future model, which can be used to determine the truth conditions of propositions grounded at different time and world indexes. 
In order to implement this, we took advantage of the classes defined in the book Computational Semantics by Jan van Eijck and Christina Unger.
The meaning of the different temporal operators is explained in the report file. 

-}


module World where

import HRAS
import P
import Lexicon
import Data.List
import Data.Maybe
import Data.Map.Strict as M


type Prop = String

type Path = [World] 

data ModalOperator = Necessarily | Possibly deriving (Show, Eq)

data TemporalOperator = X | W | F | G deriving (Show, Eq)

data PathOperator = E | A deriving (Show, Eq)

data TProp = TProp { pathOp :: PathOperator, tempOp :: TemporalOperator,
                         prop ::  [Prop] } deriving (Show, Eq)

data World = World { name :: String, propositions :: [Prop] } deriving (Eq)

data MEntail = MEntail ModalOperator (World, Prop) deriving (Show, Eq)

w1 = World { name = "w1", propositions = ["snowwhite exhales","atreyu amuses littlemook","atreyu gambles","snowwhite arrives","littlemook finds snowwhite","atreyu knows snowwhite","littlemook sits", "no princess chases a wizard"] }
w2 = World { name = "w2", propositions = ["dorothy flatters snowwhite","atreyu amuses littlemook","littlemook shaves a giant","alice avoids atreyu","dorothy enters","dorothy hears atreyu","atreyu knows snowwhite","littlemook sits", "snowwhite bans all swords", "no princess chases a wizard"] }
w3 = World { name = "w3", propositions = ["atreyu chases dorothy","snowwhite rejects dorothy","atreyu amuses littlemook","goldilocks shouts","the giant decays","the wizard sleeps","dorothy hears atreyu","atreyu knows snowwhite","goldilocks remembers atreyu","littlemook sits","alice speaks","atreyu tests snowwhite", "no princess chases a wizard", "snowwhite bans all swords", "the wizard spits"] }
w4 = World { name = "w4", propositions = ["snowwhite explodes","atreyu amuses littlemook","dorothy rests","alice spits","the wizard sleeps","goldilocks believes alice","snowwhite helps alice","atreyu knows snowwhite","goldilocks remembers atreyu","littlemook sits","snowwhite bans all swords", "some dwarf cries", "some princess chases a wizard"] }
w5 = World { name = "w5", propositions = ["dorothy dances","atreyu astounds littlemook","atreyu amuses littlemook","alice flies","goldilocks screams","the wizard sleeps","littlemook follows alice","atreyu knows snowwhite","alice misses goldilocks","goldilocks remembers atreyu", "some dwarf cries"] }

w6 = World { name = "w6", propositions = ["snowwhite hears littlemook","atreyu amuses littlemook", "goldilocks adopts some dwarfs", "snowwhite bans all swords", "every boy handle a dagger", "a sword kills atreyu", "alice receives a dagger", "littlemook pities most men", "goldilocks screams", "atreyu boasts", "some dwarf cries", "the wizard spits"] }
w7 = World { name = "w7", propositions = ["atreyu amuses littlemook", "snowwhite bans all swords", "every boy handle a dagger", "alice receives a dagger", "littlemook pities most men", "a wizard comes", "alice speaks", "some dwarf cries", "dorothy rests", "some princess chases a wizard" ] }
w8 = World { name = "w8", propositions = ["atreyu amuses littlemook", "goldilocks adopts some dwarfs", "some giant astounds dorothy", "a sword kills atreyu", "dorothy races a giant", "littlemook pities most men", "alice avoids atreyu", "alice speaks", "dorothy rests", "some giant sleeps"] }
w9 = World { name = "w9", propositions = ["the wizard spits","atreyu chases dorothy","atreyu amuses littlemook", "alice alarms most men", "no princess chases a wizard", "some giant astounds dorothy", "a sword kills atreyu", "dorothy races a giant", "snowwhite exhales", "alice speaks", "some giant sleeps", "dorothy rests" ] }
w10 = World { name = "w10", propositions = ["the wizard spits","snowwhite hears littlemook","atreyu amuses littlemook", "goldilocks adopts some dwarfs", "some giant astounds dorothy", "alice receives a dagger", "dorothy races a giant", "a wizard comes", "snowwhite exhales", "alice avoids atreyu", "dorothy rests", "alice spits"] }
w11 = World { name = "w11", propositions = ["atreyu chases dorothy","atreyu amuses littlemook", "snowwhite bans all swords", "no princess chases a wizard", "some giant astounds dorothy", "littlemook pities most men", "snowwhite exhales", "alice avoids atreyu", "dorothy remembers snowwhite", "alice spits", "the giant decays"] }
w12 = World { name = "w12", propositions = ["snowwhite hears littlemook","atreyu amuses littlemook", "alice alarms most men", "every boy handle a dagger", "some giant astounds dorothy", "a sword kills atreyu", "alice receives a dagger", "a wizard comes", "some giant screams", "dorothy remembers snowwhite", "the sword breaks"] }


model = [(w1,w2),(w2,w3),(w3,w4),(w3,w5),(w3,w6),(w4,w7),(w5,w8),(w6,w9),(w6,w10),(w10,w11),(w10,w12)]
worldList = [w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12]

instance Show World where
        show w = name w

--Checks whether the given temporal proposition holds in all possible worlds
isValid :: TProp -> Bool
isValid t = length [possW | possW <- worldList, isSatisfied t possW] == length worldList

--Checks whether the given temporal proposition holds in any of the possible worlds
isSatisfiable :: TProp -> Bool
isSatisfiable t = length [possW | possW <- worldList,isSatisfied t possW] > 0

--Checks whether the given temporal proposition holds in the given world
--For E - need to find one path out of all possible paths from given world for which the proposition holds
--For A - need to check every possible path from given world and confirm that the proposition holds for each one 
isSatisfied :: TProp -> World -> Bool
isSatisfied t w = case po of E -> pathCount > 0
                             A -> pathCount == length paths
        where po = pathOp t
              paths = futurePaths w
              pathCount = length [possPath | possPath <- paths, isSatisfiedPath isTrue t t possPath]
              
--Given two temporal propositions and a path, as well as an evaluation function, determines whether the path satisfies the proposition
isSatisfiedPath :: (TProp -> World -> Bool) -> TProp -> TProp -> Path -> Bool
isSatisfiedPath f tc t [] = case to of X -> False
                                       F -> False
                                       G -> True
                                       W -> True
        where to = tempOp tc
isSatisfiedPath f tc t (x:[]) = case to of X -> False
                                           F -> f t x
                                           G -> f t x
                                           W -> untilStepOne || untilStepTwo
        where to = tempOp tc
              secondaryp = TProp A X [last $ prop t]
              untilStepOne = ((f t x) && (not $ f secondaryp x))
              untilStepTwo = ((f secondaryp x) && (not $ f t x))
isSatisfiedPath f tc t (x:xs) = case to of X -> f t (head xs)
                                           F -> (f t x) || (isSatisfiedPath f tc t xs)
                                           G -> (f t x) && (isSatisfiedPath f tc t xs)
                                           W -> (untilStepOne && (isSatisfiedPath f tc t xs)) || (untilStepTwo && ((isSatisfiedPath f secondaryTrue secondaryTrue xs) && (not $ isSatisfiedPath f mainFalse mainFalse xs)))
        where to = tempOp tc
              secondaryp = TProp A X [last $ prop t]
              untilStepOne = ((f t x) && (not $ f secondaryp x))
              untilStepTwo = ((f secondaryp x) && (not $ f t x))
              mainFalse = TProp E F [head $ prop t]
              secondaryTrue = TProp E G $ prop secondaryp

--Functions to deal with nested temporal operators
isValidDouble :: TProp -> TProp -> Bool
isValidDouble tc t = length [possW | possW <- worldList, isSatisfiedDouble tc t possW] == length worldList

isSatisfiableDouble :: TProp -> TProp -> Bool
isSatisfiableDouble tc t = length [possW | possW <- worldList, isSatisfiedDouble tc t possW] > 0
              
isSatisfiedDouble :: TProp -> TProp -> World -> Bool
isSatisfiedDouble tc t w = case po of E -> pathCount > 0
                                      A -> pathCount == length paths
        where po = pathOp tc
              paths = futurePaths w
              pathCount = length [possPath | possPath <- paths, isSatisfiedPath isSatisfied tc t possPath]

--finds all immediate next worlds for the given world
nextWorlds :: World -> [World]
nextWorlds w = [y | (x,y) <- model, x == w]

--finds all paths starting from the current world
futurePaths :: World -> [Path]
futurePaths w | length (nextWorlds w) == 0 = [[w]]
              | otherwise = concat [Data.List.map ((++) [w]) (futurePaths x) | x <- (nextWorlds w)]

isTrue :: TProp -> World -> Bool
isTrue t w
    | (P.process p) `elem` (Data.List.map P.process (propositions w)) = True
    | otherwise = False
    where p = head $ prop t

--Implementation of boolean operators over TProps
negation :: TProp -> World -> Bool
negation p1 w = not (isSatisfied p1 w)

conj :: TProp -> TProp -> World -> Bool
conj p1 p2 w = (isSatisfied p1 w) && (isSatisfied p2 w)

disj :: TProp -> TProp -> World -> Bool
disj p1 p2 w = (isSatisfied p1 w) || (isSatisfied p2 w)

impl :: TProp -> TProp -> World -> Bool
impl p1 p2 w = (negation p1 w) || (isSatisfied p2 w) 

bicond :: TProp -> TProp -> World -> Bool
bicond p1 p2 w = (impl p1 p2 w) && (impl p2 p1 w)

-------------------------------
-- For model-checking presentation purposes:

exampleSentence = TProp A X ["the wizard spits"]
exampleC = TProp E F []

exampleSents = [ (TProp A G ["atreyu amuses littlemook"], w1), (TProp A G ["atreyu chases dorothy"], w1), (TProp E G ["the wizard spits"], w3), (TProp A G ["the wizard spits"], w3), (TProp E F ["some giant screams"], w6), (TProp A F ["some giant screams"], w6), (TProp A X ["some dwarf cries"], w3), (TProp A X ["alice receives a dagger"], w3), (TProp E X ["alice receives a dagger"], w3), (TProp E W ["no princess chases a wizard", "some princess chases a wizard"], w2), (TProp A W ["no princess chases a wizard", "some princess chases a wizard"], w2) ]

exampleDoubleSents = [ (exampleC, exampleSentence, w2), (TProp A F [], TProp E G ["atreyu amuses littlemook"], w1), (TProp A X [], TProp A G ["snowwhite bans all swords"], w1), (TProp A X [], TProp E G ["snowwhite bans all swords"], w1) ]


testSatisfied :: (TProp,World) -> IO()
testSatisfied (tprop,world) = let a = isSatisfied tprop world
         in putStrLn (show (pathOp tprop) ++ " " ++ show (tempOp tprop) ++ " " 
                        ++ show (prop tprop) ++ " " ++ show world ++ " = " ++ show a)  


testSatisfiedDouble :: (TProp, TProp, World) -> IO()
testSatisfiedDouble (tprop1, tprop2, w) = let b = isSatisfiedDouble tprop1 tprop2 w 
         in putStrLn (show (pathOp tprop1) ++ " " ++ show (tempOp tprop1) ++ " " 
                        ++ show (pathOp tprop2) ++ " " ++show (tempOp tprop2) ++ " " 
                        ++ show (prop tprop2) ++ " " ++ show w ++ " = " ++ show b) 



-- 'execute' takes a function and a list and DOES SOME IO.
execute :: ( (TProp,World) -> IO()) -> [(TProp,World)] -> IO () 
execute _ [] = putStrLn "Done!"
execute f (x:xs) = do f x ; execute f xs

executeDouble :: ( (TProp,TProp,World) -> IO()) -> [(TProp,TProp,World)] -> IO () 
executeDouble _ [] = putStrLn "Done!"
executeDouble f (x:xs) = do f x ; executeDouble f xs

main = do{
          putStrLn "";
          putStrLn "Testing isSatisfied...";
          execute testSatisfied exampleSents;
          putStrLn "";

          putStrLn "Testing isSatisfiedDouble...";
          executeDouble testSatisfiedDouble exampleDoubleSents;
          putStrLn ""; 
        }

