{-
Dan Noar
David Rubio Vallejo

This module defines a lexicon of available terms to be used in World.hs

-}

module Lexicon where

type CatLabel = String
type Phon     = String

data Cat      = Cat Phon CatLabel Agreement [Cat]
              deriving Eq

type Agreement = [Feat]

data Feat = Masc  | Fem  | Neutr | MascOrFem 
          | Sg    | Pl 
          | Fst   | Snd  | Thrd 
          | Nom   | AccOrDat 
          | Pers  | Refl | Wh 
          | Past  | Pres | Fut   | Perf | Infl
          | On    | With | By    | To   | From  
          deriving (Eq,Show,Ord)

lexicon :: String -> [Cat]

lexicon "i"   = [Cat "i"   "NP" [Pers,Fst,Sg,Nom]        []]
lexicon "me"  = [Cat "me"  "NP" [Pers,Fst,Sg,AccOrDat]   []]
lexicon "we"  = [Cat "we"  "NP" [Pers,Fst,Pl,Nom]        []]
lexicon "us"  = [Cat "us"  "NP" [Pers,Fst,Pl,AccOrDat]   []]
lexicon "you" = [Cat "you" "NP" [Pers,Snd]               []]
lexicon "he"  = [Cat "he"  "NP" [Pers,Thrd,Sg,Nom,Masc]  []]
lexicon "him" = [Cat "him" "NP" [Pers,Thrd,Sg,AccOrDat,Masc] 
	      	     	    []]
lexicon "she" = [Cat "she" "NP" [Pers,Thrd,Sg,Nom,Fem]   []]
lexicon "her" = [Cat "her" "NP" [Pers,Thrd,Sg,AccOrDat,Fem] 
	      	     	    []]
lexicon "it"  = [Cat "it"  "NP" [Pers,Thrd,Sg,Neutr]     []]
lexicon "they" = [Cat "they" "NP" [Pers,Thrd,Pl,Nom]     []]
lexicon "them" = [Cat "them" "NP" [Pers,Thrd,Pl,AccOrDat] 
	       	      	      []]

lexicon "myself"     = 
 [Cat "myself"     "NP" [Refl,Sg,Fst,AccOrDat] []]
lexicon "ourselves"  = 
 [Cat "ourselves"  "NP" [Refl,Pl,Fst,AccOrDat] []]
lexicon "yourself"   = 
 [Cat "yourself"   "NP" [Refl,Sg,Snd,AccOrDat] []]
lexicon "yourselves" = 
 [Cat "yourselves" "NP" [Refl,Pl,Snd,AccOrDat] []]
lexicon "himself"    = 
 [Cat "himself"    "NP" [Refl,Sg,Thrd,AccOrDat,Masc]  []]
lexicon "herself"    = 
 [Cat "herself"    "NP" [Refl,Sg,Thrd,AccOrDat,Fem]   []]
lexicon "itself"     = 
 [Cat "itself"     "NP" [Refl,Sg,Thrd,AccOrDat,Neutr] []]
lexicon "themselves" = 
 [Cat "themselves" "NP" [Refl,Pl,Thrd,AccOrDat] []]

lexicon "who"     = [Cat "who" "NP"  [Wh,Thrd,MascOrFem] [], 
     Cat "who" "REL" [MascOrFem]         []]
lexicon "whom"    = 
 [Cat "whom" "NP"  [Sg,Wh,Thrd,AccOrDat,MascOrFem] [], 
  Cat "whom" "REL" [Sg,MascOrFem,AccOrDat]         []]
lexicon "what"    = 
 [Cat "what" "NP"  [Wh,Thrd,AccOrDat,Neutr]    []]
lexicon "that"    = [Cat "that"  "REL" []      [], 
                     Cat "that"  "DET" [Sg]    []]
lexicon "which"   = [Cat "which" "REL" [Neutr] [], 
                     Cat "which" "DET" [Wh]    []]

lexicon "snowwhite"    = 
 [Cat "snowwhite"  "NP" [Thrd,Fem,Sg]  []]
lexicon "alice"        = 
 [Cat "alice"      "NP" [Thrd,Fem,Sg]  []]
lexicon "dorothy"      = 
 [Cat "dorothy"    "NP" [Thrd,Fem,Sg]  []]
lexicon "goldilocks"   = 
 [Cat "goldilocks" "NP" [Thrd,Fem,Sg]  []]
lexicon "littlemook"   = 
 [Cat "littlemook" "NP" [Thrd,Masc,Sg] []]
lexicon "atreyu"       = 
 [Cat "atreyu"     "NP" [Thrd,Masc,Sg] []]

lexicon "every"   = [Cat "every"   "DET" [Sg]  []]
lexicon "all"     = [Cat "all"     "DET" [Pl]  []]
lexicon "some"    = [Cat "some"    "DET" []    []]
lexicon "several" = [Cat "several" "DET" [Pl]  []]
lexicon "a"       = [Cat "a"       "DET" [Sg]  []]
lexicon "no"      = [Cat "no"      "DET" []    []]
lexicon "the"     = [Cat "the"     "DET" []    []]

lexicon "most"    = [Cat "most"    "DET" [Pl]  []]
lexicon "many"    = [Cat "many"    "DET" [Pl]  []]
lexicon "few"     = [Cat "few"     "DET" [Pl]  []]
lexicon "this"    = [Cat "this"    "DET" [Sg]  []]
lexicon "these"   = [Cat "these"   "DET" [Pl]  []]
lexicon "those"   = [Cat "those"   "DET" [Pl]  []]

lexicon "less_than" = [Cat "less_than" "DF" [Pl] []]
lexicon "more_than" = [Cat "more_than" "DF" [Pl] []]

lexicon "thing"   = [Cat "thing"   "CN" [Sg,Neutr,Thrd] []]
lexicon "things"  = [Cat "things"  "CN" [Pl,Neutr,Thrd] []]
lexicon "person"  = [Cat "person"  "CN" [Sg,Masc,Thrd]  []]
lexicon "persons" = [Cat "persons" "CN" [Pl,Masc,Thrd]  []]
lexicon "boy"     = [Cat "boy"     "CN" [Sg,Masc,Thrd]  []]
lexicon "boys"    = [Cat "boys"    "CN" [Pl,Masc,Thrd]  []]
lexicon "man"     = [Cat "man"     "CN" [Sg,Masc,Thrd]  []]
lexicon "men"     = [Cat "men"     "CN" [Pl,Masc,Thrd]  []]
lexicon "girl"    = [Cat "girl"    "CN" [Sg,Fem,Thrd]   []]
lexicon "girls"   = [Cat "girls"   "CN" [Pl,Fem,Thrd]   []]
lexicon "woman"   = [Cat "woman"   "CN" [Sg,Fem,Thrd]   []]
lexicon "women"   = [Cat "women"   "CN" [Pl,Fem,Thrd]   []]
lexicon "princess" = [Cat "princess" "CN" [Sg,Fem,Thrd] []]
lexicon "princesses" = [Cat "princesses" "CN" [Pl,Fem,Thrd] []]
lexicon "dwarf"    = [Cat "dwarf"    "CN" [Sg,Masc,Thrd] []]
lexicon "dwarfs"   = [Cat "dwarfs"   "CN" [Pl,Masc,Thrd] []]
lexicon "dwarves"  = [Cat "dwarves"  "CN" [Pl,Masc,Thrd] []]
lexicon "giant"    = [Cat "giant"    "CN" [Sg,Masc,Thrd] []]
lexicon "giants"   = [Cat "giants"   "CN" [Pl,Masc,Thrd] []]

lexicon "wizard"   = [Cat "wizard"   "CN" [Sg,Masc,Thrd]  []]
lexicon "wizards"  = [Cat "wizards"  "CN" [Pl,Masc,Thrd]  []]
lexicon "sword"    = [Cat "sword"    "CN" [Sg,Neutr,Thrd] []]
lexicon "swords"   = [Cat "swords"   "CN" [Pl,Neutr,Thrd] []]
lexicon "dagger"   = [Cat "dagger"   "CN" [Sg,Neutr,Thrd] []]
lexicon "daggers"  = [Cat "daggers"  "CN" [Pl,Neutr,Thrd] []]

lexicon "did"    = [Cat "did"    "AUX" [] []]
lexicon "didn't" = [Cat "didn't" "AUX" [] []]

lexicon "smiled"    = [Cat "smiled"    "VP" [Past] []]
lexicon "smile"     = [Cat "smile"     "VP" [Infl]  []]
lexicon "laughed"   = [Cat "laughed"   "VP" [Past] []]
lexicon "laugh"     = [Cat "laugh"     "VP" [Infl]  []]
lexicon "cheered"   = [Cat "cheered"   "VP" [Past] []]
lexicon "cheer"     = [Cat "cheer"     "VP" [Infl]  []]
lexicon "shuddered" = [Cat "shuddered" "VP" [Past] []]
lexicon "shudder"   = [Cat "shudder"   "VP" [Infl]  []]

lexicon "loved"        = 
 [Cat "loved"    "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "love"         = 
 [Cat "love"     "VP" [Infl]  [Cat "_" "NP" [AccOrDat] []]]
lexicon "admired"      = 
 [Cat "admired"  "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "admire"       = 
 [Cat "admire"   "VP" [Infl]  [Cat "_" "NP" [AccOrDat] []]]

lexicon "helped"       = 
 [Cat "helped"   "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "help"         = 
 [Cat "help"     "VP" [Infl]  [Cat "_" "NP" [AccOrDat] []]]
lexicon "defeated"       = 
 [Cat "defeated" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "defeat"         = 
 [Cat "defeat"   "VP" [Infl]  [Cat "_" "NP" [AccOrDat] []]]

lexicon "gave"         = 
 [Cat "gave" "VP" [Past] [Cat "_" "NP" [AccOrDat] [],
                    Cat "_" "PP" [To]       []], 
  Cat "gave" "VP" [Past] [Cat "_" "NP" [AccOrDat] [],
                     Cat "_" "NP" [AccOrDat]  []]]
lexicon "give"         = 
 [Cat "give" "VP" [Infl]  [Cat "_" "NP" [AccOrDat] [],
                           Cat "_" "PP" [To]       []],
  Cat "give" "VP" [Infl]  [Cat "_" "NP" [AccOrDat] [],
                           Cat "_" "NP" [AccOrDat] []]]
lexicon "sold" = 
 [Cat "sold" "VP" [Past] [Cat "_" "NP" [AccOrDat] [],
                           Cat "_" "PP" [To]       []],
  Cat "sold" "VP" [Past] [Cat "_" "NP" [AccOrDat] [],
                           Cat "_" "NP" [AccOrDat] []]]
lexicon "sell" = 
 [Cat "sell" "VP" [Infl]  [Cat "_" "NP" [AccOrDat] [],
                           Cat "_" "PP" [To]       []],
  Cat "sell" "VP" [Infl]  [Cat "_" "NP" [AccOrDat] [],
                           Cat "_" "NP" [AccOrDat] []]]

lexicon "kicked" = 
 [Cat "kicked" "VP" [Past] [Cat "_" "NP" [AccOrDat] [],
                             Cat "_" "PP" [With]     []], 
  Cat "kicked" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "kick" = 
 [Cat "kick"   "VP" [Infl]  [Cat "_" "NP" [AccOrDat] [],
                      Cat "_" "PP" [With]     []], 
  Cat "kick"   "VP" [Infl]  [Cat "_" "NP" [AccOrDat] []]] 

lexicon "took" = 
 [Cat "took" "VP" [Past] [Cat "_" "NP" [AccOrDat] [],
                    Cat "_" "PP" [From]     []], 
  Cat "took" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "take" = 
 [Cat "take" "VP" [Infl]  [Cat "_" "NP" [AccOrDat] [],
                           Cat "_" "PP" [From]     []], 
  Cat "take" "VP" [Infl]  [Cat "_" "NP" [AccOrDat] []]] 

lexicon "on"   = [Cat "on"   "PREP" [On]   []]
lexicon "with" = [Cat "with" "PREP" [With] []]
lexicon "by"   = [Cat "by"   "PREP" [By]   []]
lexicon "to"   = [Cat "to"   "PREP" [To]   []]
lexicon "from" = [Cat "from" "PREP" [From] []]

lexicon "and"   = [Cat "and"  "CONJ" [] []]
lexicon "."     = [Cat "."    "CONJ" [] []]
lexicon "if"    = [Cat "if"   "COND" [] []]
lexicon "then"  = [Cat "then" "THEN" [] []]

lexicon "shouted"    = [Cat "shouted"    "VP" [Past] []]
lexicon "shout"    = [Cat "shout"    "VP" [Pres,Sg,Fst] [],
	Cat "shout"    "VP" [Pres,Sg,Snd] [],
	Cat "shout"     "VP" [Pres,Pl]  [],
	Cat "shout"     "VP" [Infl]  []]
lexicon "shouts"    = [Cat "shouts"    "VP" [Pres,Sg,Thrd] []]
lexicon "will_shout"    = [Cat "will_shout"    "VP" [Fut] []]
lexicon "have_shouted"	= [Cat "have_shouted"  "VP" [Perf,Sg,Fst] [],
	Cat "have_shouted"     "VP" [Perf,Sg,Snd] [],
	Cat "have_shouted"     "VP" [Perf,Pl] []]
lexicon "has_shouted"   = [Cat "has_shouted"   "VP" [Perf,Sg,Thrd] []]


lexicon "aged" = [Cat "aged" "VP" [Past] []]
lexicon "age" = [Cat "age" "VP" [Pres,Sg,Fst] [],
	Cat "age" "VP" [Pres,Sg,Snd] [],
	Cat "age" "VP" [Pres,Pl] [],
	Cat "age" "VP" [Infl] []]
lexicon "ages" = [Cat "ages" "VP" [Pres,Sg,Thrd] []]
lexicon "will_age" = [Cat "will_age" "VP" [Fut] []]
lexicon "have_aged" = [Cat "have_aged" "VP" [Perf,Sg,Fst] [],
	Cat "have_aged" "VP" [Perf,Sg,Snd] [],
	Cat "have_aged" "VP" [Perf,Pl] []]
lexicon "has_aged" = [Cat "has_aged" "VP" [Perf,Sg,Thrd] []]

lexicon "arrived" = [Cat "arrived" "VP" [Past] []]
lexicon "arrive" = [Cat "arrive" "VP" [Pres,Sg,Fst] [],
	Cat "arrive" "VP" [Pres,Sg,Snd] [],
	Cat "arrive" "VP" [Pres,Pl] [],
	Cat "arrive" "VP" [Infl] []]
lexicon "arrives" = [Cat "arrives" "VP" [Pres,Sg,Thrd] []]
lexicon "will_arrive" = [Cat "will_arrive" "VP" [Fut] []]
lexicon "have_arrived" = [Cat "have_arrived" "VP" [Perf,Sg,Fst] [],
	Cat "have_arrived" "VP" [Perf,Sg,Snd] [],
	Cat "have_arrived" "VP" [Perf,Pl] []]
lexicon "has_arrived" = [Cat "has_arrived" "VP" [Perf,Sg,Thrd] []]

lexicon "ached" = [Cat "ached" "VP" [Past] []]
lexicon "ache" = [Cat "ache" "VP" [Pres,Sg,Fst] [],
	Cat "ache" "VP" [Pres,Sg,Snd] [],
	Cat "ache" "VP" [Pres,Pl] [],
	Cat "ache" "VP" [Infl] []]
lexicon "aches" = [Cat "aches" "VP" [Pres,Sg,Thrd] []]
lexicon "will_ache" = [Cat "will_ache" "VP" [Fut] []]
lexicon "have_ached" = [Cat "have_ached" "VP" [Perf,Sg,Fst] [],
	Cat "have_ached" "VP" [Perf,Sg,Snd] [],
	Cat "have_ached" "VP" [Perf,Pl] []]
lexicon "has_ached" = [Cat "has_ached" "VP" [Perf,Sg,Thrd] []]

lexicon "advanced" = [Cat "advanced" "VP" [Past] []]
lexicon "advance" = [Cat "advance" "VP" [Pres,Sg,Fst] [],
	Cat "advance" "VP" [Pres,Sg,Snd] [],
	Cat "advance" "VP" [Pres,Pl] [],
	Cat "advance" "VP" [Infl] []]
lexicon "advances" = [Cat "advances" "VP" [Pres,Sg,Thrd] []]
lexicon "will_advance" = [Cat "will_advance" "VP" [Fut] []]
lexicon "have_advanced" = [Cat "have_advanced" "VP" [Perf,Sg,Fst] [],
	Cat "have_advanced" "VP" [Perf,Sg,Snd] [],
	Cat "have_advanced" "VP" [Perf,Pl] []]
lexicon "has_advanced" = [Cat "has_advanced" "VP" [Perf,Sg,Thrd] []]

lexicon "ailed" = [Cat "ailed" "VP" [Past] []]
lexicon "ail" = [Cat "ail" "VP" [Pres,Sg,Fst] [],
	Cat "ail" "VP" [Pres,Sg,Snd] [],
	Cat "ail" "VP" [Pres,Pl] [],
	Cat "ail" "VP" [Infl] []]
lexicon "ails" = [Cat "ails" "VP" [Pres,Sg,Thrd] []]
lexicon "will_ail" = [Cat "will_ail" "VP" [Fut] []]
lexicon "have_ailed" = [Cat "have_ailed" "VP" [Perf,Sg,Fst] [],
	Cat "have_ailed" "VP" [Perf,Sg,Snd] [],
	Cat "have_ailed" "VP" [Perf,Pl] []]
lexicon "has_ailed" = [Cat "has_ailed" "VP" [Perf,Sg,Thrd] []]

lexicon "apologized" = [Cat "apologized" "VP" [Past] []]
lexicon "apologize" = [Cat "apologize" "VP" [Pres,Sg,Fst] [],
	Cat "apologize" "VP" [Pres,Sg,Snd] [],
	Cat "apologize" "VP" [Pres,Pl] [],
	Cat "apologize" "VP" [Infl] []]
lexicon "apologizes" = [Cat "apologizes" "VP" [Pres,Sg,Thrd] []]
lexicon "will_apologize" = [Cat "will_apologize" "VP" [Fut] []]
lexicon "have_apologized" = [Cat "have_apologized" "VP" [Perf,Sg,Fst] [],
	Cat "have_apologized" "VP" [Perf,Sg,Snd] [],
	Cat "have_apologized" "VP" [Perf,Pl] []]
lexicon "has_apologized" = [Cat "has_apologized" "VP" [Perf,Sg,Thrd] []]

lexicon "boasted" = [Cat "boasted" "VP" [Past] []]
lexicon "boast" = [Cat "boast" "VP" [Pres,Sg,Fst] [],
	Cat "boast" "VP" [Pres,Sg,Snd] [],
	Cat "boast" "VP" [Pres,Pl] [],
	Cat "boast" "VP" [Infl] []]
lexicon "boasts" = [Cat "boasts" "VP" [Pres,Sg,Thrd] []]
lexicon "will_boast" = [Cat "will_boast" "VP" [Fut] []]
lexicon "have_boasted" = [Cat "have_boasted" "VP" [Perf,Sg,Fst] [],
	Cat "have_boasted" "VP" [Perf,Sg,Snd] [],
	Cat "have_boasted" "VP" [Perf,Pl] []]
lexicon "has_boasted" = [Cat "has_boasted" "VP" [Perf,Sg,Thrd] []]

lexicon "breathed" = [Cat "breathed" "VP" [Past] []]
lexicon "breathe" = [Cat "breathe" "VP" [Pres,Sg,Fst] [],
	Cat "breathe" "VP" [Pres,Sg,Snd] [],
	Cat "breathe" "VP" [Pres,Pl] [],
	Cat "breathe" "VP" [Infl] []]
lexicon "breathes" = [Cat "breathes" "VP" [Pres,Sg,Thrd] []]
lexicon "will_breathe" = [Cat "will_breathe" "VP" [Fut] []]
lexicon "have_breathed" = [Cat "have_breathed" "VP" [Perf,Sg,Fst] [],
	Cat "have_breathed" "VP" [Perf,Sg,Snd] [],
	Cat "have_breathed" "VP" [Perf,Pl] []]
lexicon "has_breathed" = [Cat "has_breathed" "VP" [Perf,Sg,Thrd] []]

lexicon "breeded" = [Cat "breeded" "VP" [Past] []]
lexicon "breed" = [Cat "breed" "VP" [Pres,Sg,Fst] [],
	Cat "breed" "VP" [Pres,Sg,Snd] [],
	Cat "breed" "VP" [Pres,Pl] [],
	Cat "breed" "VP" [Infl] []]
lexicon "breeds" = [Cat "breeds" "VP" [Pres,Sg,Thrd] []]
lexicon "will_breed" = [Cat "will_breed" "VP" [Fut] []]
lexicon "have_breeded" = [Cat "have_breeded" "VP" [Perf,Sg,Fst] [],
	Cat "have_breeded" "VP" [Perf,Sg,Snd] [],
	Cat "have_breeded" "VP" [Perf,Pl] []]
lexicon "has_breeded" = [Cat "has_breeded" "VP" [Perf,Sg,Thrd] []]

lexicon "chanted" = [Cat "chanted" "VP" [Past] []]
lexicon "chant" = [Cat "chant" "VP" [Pres,Sg,Fst] [],
	Cat "chant" "VP" [Pres,Sg,Snd] [],
	Cat "chant" "VP" [Pres,Pl] [],
	Cat "chant" "VP" [Infl] []]
lexicon "chants" = [Cat "chants" "VP" [Pres,Sg,Thrd] []]
lexicon "will_chant" = [Cat "will_chant" "VP" [Fut] []]
lexicon "have_chanted" = [Cat "have_chanted" "VP" [Perf,Sg,Fst] [],
	Cat "have_chanted" "VP" [Perf,Sg,Snd] [],
	Cat "have_chanted" "VP" [Perf,Pl] []]
lexicon "has_chanted" = [Cat "has_chanted" "VP" [Perf,Sg,Thrd] []]

lexicon "cheated" = [Cat "cheated" "VP" [Past] []]
lexicon "cheat" = [Cat "cheat" "VP" [Pres,Sg,Fst] [],
	Cat "cheat" "VP" [Pres,Sg,Snd] [],
	Cat "cheat" "VP" [Pres,Pl] [],
	Cat "cheat" "VP" [Infl] []]
lexicon "cheats" = [Cat "cheats" "VP" [Pres,Sg,Thrd] []]
lexicon "will_cheat" = [Cat "will_cheat" "VP" [Fut] []]
lexicon "have_cheated" = [Cat "have_cheated" "VP" [Perf,Sg,Fst] [],
	Cat "have_cheated" "VP" [Perf,Sg,Snd] [],
	Cat "have_cheated" "VP" [Perf,Pl] []]
lexicon "has_cheated" = [Cat "has_cheated" "VP" [Perf,Sg,Thrd] []]

lexicon "came" = [Cat "came" "VP" [Past] []]
lexicon "come" = [Cat "come" "VP" [Pres,Sg,Fst] [],
	Cat "come" "VP" [Pres,Sg,Snd] [],
	Cat "come" "VP" [Pres,Pl] [],
	Cat "come" "VP" [Infl] []]
lexicon "comes" = [Cat "comes" "VP" [Pres,Sg,Thrd] []]
lexicon "will_come" = [Cat "will_come" "VP" [Fut] []]
lexicon "have_come" = [Cat "have_come" "VP" [Perf,Sg,Fst] [],
	Cat "have_come" "VP" [Perf,Sg,Snd] [],
	Cat "have_come" "VP" [Perf,Pl] []]
lexicon "has_come" = [Cat "has_come" "VP" [Perf,Sg,Thrd] []]

lexicon "cried" = [Cat "cried" "VP" [Past] []]
lexicon "cry" = [Cat "cry" "VP" [Pres,Sg,Fst] [],
	Cat "cry" "VP" [Pres,Sg,Snd] [],
	Cat "cry" "VP" [Pres,Pl] [],
	Cat "cry" "VP" [Infl] []]
lexicon "cries" = [Cat "cries" "VP" [Pres,Sg,Thrd] []]
lexicon "will_cry" = [Cat "will_cry" "VP" [Fut] []]
lexicon "have_cried" = [Cat "have_cried" "VP" [Perf,Sg,Fst] [],
	Cat "have_cried" "VP" [Perf,Sg,Snd] [],
	Cat "have_cried" "VP" [Perf,Pl] []]
lexicon "has_cried" = [Cat "has_cried" "VP" [Perf,Sg,Thrd] []]

lexicon "danced" = [Cat "danced" "VP" [Past] []]
lexicon "dance" = [Cat "dance" "VP" [Pres,Sg,Fst] [],
	Cat "dance" "VP" [Pres,Sg,Snd] [],
	Cat "dance" "VP" [Pres,Pl] [],
	Cat "dance" "VP" [Infl] []]
lexicon "dances" = [Cat "dances" "VP" [Pres,Sg,Thrd] []]
lexicon "will_dance" = [Cat "will_dance" "VP" [Fut] []]
lexicon "have_danced" = [Cat "have_danced" "VP" [Perf,Sg,Fst] [],
	Cat "have_danced" "VP" [Perf,Sg,Snd] [],
	Cat "have_danced" "VP" [Perf,Pl] []]
lexicon "has_danced" = [Cat "has_danced" "VP" [Perf,Sg,Thrd] []]

lexicon "decayed" = [Cat "decayed" "VP" [Past] []]
lexicon "decay" = [Cat "decay" "VP" [Pres,Sg,Fst] [],
	Cat "decay" "VP" [Pres,Sg,Snd] [],
	Cat "decay" "VP" [Pres,Pl] [],
	Cat "decay" "VP" [Infl] []]
lexicon "decays" = [Cat "decays" "VP" [Pres,Sg,Thrd] []]
lexicon "will_decay" = [Cat "will_decay" "VP" [Fut] []]
lexicon "have_decayed" = [Cat "have_decayed" "VP" [Perf,Sg,Fst] [],
	Cat "have_decayed" "VP" [Perf,Sg,Snd] [],
	Cat "have_decayed" "VP" [Perf,Pl] []]
lexicon "has_decayed" = [Cat "has_decayed" "VP" [Perf,Sg,Thrd] []]

lexicon "delayed" = [Cat "delayed" "VP" [Past] []]
lexicon "delay" = [Cat "delay" "VP" [Pres,Sg,Fst] [],
	Cat "delay" "VP" [Pres,Sg,Snd] [],
	Cat "delay" "VP" [Pres,Pl] [],
	Cat "delay" "VP" [Infl] []]
lexicon "delays" = [Cat "delays" "VP" [Pres,Sg,Thrd] []]
lexicon "will_delay" = [Cat "will_delay" "VP" [Fut] []]
lexicon "have_delayed" = [Cat "have_delayed" "VP" [Perf,Sg,Fst] [],
	Cat "have_delayed" "VP" [Perf,Sg,Snd] [],
	Cat "have_delayed" "VP" [Perf,Pl] []]
lexicon "has_delayed" = [Cat "has_delayed" "VP" [Perf,Sg,Thrd] []]

lexicon "deliberated" = [Cat "deliberated" "VP" [Past] []]
lexicon "deliberate" = [Cat "deliberate" "VP" [Pres,Sg,Fst] [],
	Cat "deliberate" "VP" [Pres,Sg,Snd] [],
	Cat "deliberate" "VP" [Pres,Pl] [],
	Cat "deliberate" "VP" [Infl] []]
lexicon "deliberates" = [Cat "deliberates" "VP" [Pres,Sg,Thrd] []]
lexicon "will_deliberate" = [Cat "will_deliberate" "VP" [Fut] []]
lexicon "have_deliberated" = [Cat "have_deliberated" "VP" [Perf,Sg,Fst] [],
	Cat "have_deliberated" "VP" [Perf,Sg,Snd] [],
	Cat "have_deliberated" "VP" [Perf,Pl] []]
lexicon "has_deliberated" = [Cat "has_deliberated" "VP" [Perf,Sg,Thrd] []]

lexicon "depreciated" = [Cat "depreciated" "VP" [Past] []]
lexicon "depreciate" = [Cat "depreciate" "VP" [Pres,Sg,Fst] [],
	Cat "depreciate" "VP" [Pres,Sg,Snd] [],
	Cat "depreciate" "VP" [Pres,Pl] [],
	Cat "depreciate" "VP" [Infl] []]
lexicon "depreciates" = [Cat "depreciates" "VP" [Pres,Sg,Thrd] []]
lexicon "will_depreciate" = [Cat "will_depreciate" "VP" [Fut] []]
lexicon "have_depreciated" = [Cat "have_depreciated" "VP" [Perf,Sg,Fst] [],
	Cat "have_depreciated" "VP" [Perf,Sg,Snd] [],
	Cat "have_depreciated" "VP" [Perf,Pl] []]
lexicon "has_depreciated" = [Cat "has_depreciated" "VP" [Perf,Sg,Thrd] []]

lexicon "entered" = [Cat "entered" "VP" [Past] []]
lexicon "enter" = [Cat "enter" "VP" [Pres,Sg,Fst] [],
	Cat "enter" "VP" [Pres,Sg,Snd] [],
	Cat "enter" "VP" [Pres,Pl] [],
	Cat "enter" "VP" [Infl] []]
lexicon "enters" = [Cat "enters" "VP" [Pres,Sg,Thrd] []]
lexicon "will_enter" = [Cat "will_enter" "VP" [Fut] []]
lexicon "have_entered" = [Cat "have_entered" "VP" [Perf,Sg,Fst] [],
	Cat "have_entered" "VP" [Perf,Sg,Snd] [],
	Cat "have_entered" "VP" [Perf,Pl] []]
lexicon "has_entered" = [Cat "has_entered" "VP" [Perf,Sg,Thrd] []]

lexicon "escaped" = [Cat "escaped" "VP" [Past] []]
lexicon "escape" = [Cat "escape" "VP" [Pres,Sg,Fst] [],
	Cat "escape" "VP" [Pres,Sg,Snd] [],
	Cat "escape" "VP" [Pres,Pl] [],
	Cat "escape" "VP" [Infl] []]
lexicon "escapes" = [Cat "escapes" "VP" [Pres,Sg,Thrd] []]
lexicon "will_escape" = [Cat "will_escape" "VP" [Fut] []]
lexicon "have_escaped" = [Cat "have_escaped" "VP" [Perf,Sg,Fst] [],
	Cat "have_escaped" "VP" [Perf,Sg,Snd] [],
	Cat "have_escaped" "VP" [Perf,Pl] []]
lexicon "has_escaped" = [Cat "has_escaped" "VP" [Perf,Sg,Thrd] []]

lexicon "exhaled" = [Cat "exhaled" "VP" [Past] []]
lexicon "exhale" = [Cat "exhale" "VP" [Pres,Sg,Fst] [],
	Cat "exhale" "VP" [Pres,Sg,Snd] [],
	Cat "exhale" "VP" [Pres,Pl] [],
	Cat "exhale" "VP" [Infl] []]
lexicon "exhales" = [Cat "exhales" "VP" [Pres,Sg,Thrd] []]
lexicon "will_exhale" = [Cat "will_exhale" "VP" [Fut] []]
lexicon "have_exhaled" = [Cat "have_exhaled" "VP" [Perf,Sg,Fst] [],
	Cat "have_exhaled" "VP" [Perf,Sg,Snd] [],
	Cat "have_exhaled" "VP" [Perf,Pl] []]
lexicon "has_exhaled" = [Cat "has_exhaled" "VP" [Perf,Sg,Thrd] []]

lexicon "exploded" = [Cat "exploded" "VP" [Past] []]
lexicon "explode" = [Cat "explode" "VP" [Pres,Sg,Fst] [],
	Cat "explode" "VP" [Pres,Sg,Snd] [],
	Cat "explode" "VP" [Pres,Pl] [],
	Cat "explode" "VP" [Infl] []]
lexicon "explodes" = [Cat "explodes" "VP" [Pres,Sg,Thrd] []]
lexicon "will_explode" = [Cat "will_explode" "VP" [Fut] []]
lexicon "have_exploded" = [Cat "have_exploded" "VP" [Perf,Sg,Fst] [],
	Cat "have_exploded" "VP" [Perf,Sg,Snd] [],
	Cat "have_exploded" "VP" [Perf,Pl] []]
lexicon "has_exploded" = [Cat "has_exploded" "VP" [Perf,Sg,Thrd] []]

lexicon "failed" = [Cat "failed" "VP" [Past] []]
lexicon "fail" = [Cat "fail" "VP" [Pres,Sg,Fst] [],
	Cat "fail" "VP" [Pres,Sg,Snd] [],
	Cat "fail" "VP" [Pres,Pl] [],
	Cat "fail" "VP" [Infl] []]
lexicon "fails" = [Cat "fails" "VP" [Pres,Sg,Thrd] []]
lexicon "will_fail" = [Cat "will_fail" "VP" [Fut] []]
lexicon "have_failed" = [Cat "have_failed" "VP" [Perf,Sg,Fst] [],
	Cat "have_failed" "VP" [Perf,Sg,Snd] [],
	Cat "have_failed" "VP" [Perf,Pl] []]
lexicon "has_failed" = [Cat "has_failed" "VP" [Perf,Sg,Thrd] []]

lexicon "flew" = [Cat "flew" "VP" [Past] []]
lexicon "fly" = [Cat "fly" "VP" [Pres,Sg,Fst] [],
	Cat "fly" "VP" [Pres,Sg,Snd] [],
	Cat "fly" "VP" [Pres,Pl] [],
	Cat "fly" "VP" [Infl] []]
lexicon "flies" = [Cat "flies" "VP" [Pres,Sg,Thrd] []]
lexicon "will_fly" = [Cat "will_fly" "VP" [Fut] []]
lexicon "have_flown" = [Cat "have_flown" "VP" [Perf,Sg,Fst] [],
	Cat "have_flown" "VP" [Perf,Sg,Snd] [],
	Cat "have_flown" "VP" [Perf,Pl] []]
lexicon "has_flown" = [Cat "has_flown" "VP" [Perf,Sg,Thrd] []]

lexicon "functioned" = [Cat "functioned" "VP" [Past] []]
lexicon "function" = [Cat "function" "VP" [Pres,Sg,Fst] [],
	Cat "function" "VP" [Pres,Sg,Snd] [],
	Cat "function" "VP" [Pres,Pl] [],
	Cat "function" "VP" [Infl] []]
lexicon "functions" = [Cat "functions" "VP" [Pres,Sg,Thrd] []]
lexicon "will_function" = [Cat "will_function" "VP" [Fut] []]
lexicon "have_functioned" = [Cat "have_functioned" "VP" [Perf,Sg,Fst] [],
	Cat "have_functioned" "VP" [Perf,Sg,Snd] [],
	Cat "have_functioned" "VP" [Perf,Pl] []]
lexicon "has_functioned" = [Cat "has_functioned" "VP" [Perf,Sg,Thrd] []]

lexicon "gambled" = [Cat "gambled" "VP" [Past] []]
lexicon "gamble" = [Cat "gamble" "VP" [Pres,Sg,Fst] [],
	Cat "gamble" "VP" [Pres,Sg,Snd] [],
	Cat "gamble" "VP" [Pres,Pl] [],
	Cat "gamble" "VP" [Infl] []]
lexicon "gambles" = [Cat "gambles" "VP" [Pres,Sg,Thrd] []]
lexicon "will_gamble" = [Cat "will_gamble" "VP" [Fut] []]
lexicon "have_gambled" = [Cat "have_gambled" "VP" [Perf,Sg,Fst] [],
	Cat "have_gambled" "VP" [Perf,Sg,Snd] [],
	Cat "have_gambled" "VP" [Perf,Pl] []]
lexicon "has_gambled" = [Cat "has_gambled" "VP" [Perf,Sg,Thrd] []]

lexicon "happened" = [Cat "happened" "VP" [Past] []]
lexicon "happen" = [Cat "happen" "VP" [Pres,Sg,Fst] [],
	Cat "happen" "VP" [Pres,Sg,Snd] [],
	Cat "happen" "VP" [Pres,Pl] [],
	Cat "happen" "VP" [Infl] []]
lexicon "happens" = [Cat "happens" "VP" [Pres,Sg,Thrd] []]
lexicon "will_happen" = [Cat "will_happen" "VP" [Fut] []]
lexicon "have_happened" = [Cat "have_happened" "VP" [Perf,Sg,Fst] [],
	Cat "have_happened" "VP" [Perf,Sg,Snd] [],
	Cat "have_happened" "VP" [Perf,Pl] []]
lexicon "has_happened" = [Cat "has_happened" "VP" [Perf,Sg,Thrd] []]

lexicon "hesitated" = [Cat "hesitated" "VP" [Past] []]
lexicon "hesitate" = [Cat "hesitate" "VP" [Pres,Sg,Fst] [],
	Cat "hesitate" "VP" [Pres,Sg,Snd] [],
	Cat "hesitate" "VP" [Pres,Pl] [],
	Cat "hesitate" "VP" [Infl] []]
lexicon "hesitates" = [Cat "hesitates" "VP" [Pres,Sg,Thrd] []]
lexicon "will_hesitate" = [Cat "will_hesitate" "VP" [Fut] []]
lexicon "have_hesitated" = [Cat "have_hesitated" "VP" [Perf,Sg,Fst] [],
	Cat "have_hesitated" "VP" [Perf,Sg,Snd] [],
	Cat "have_hesitated" "VP" [Perf,Pl] []]
lexicon "has_hesitated" = [Cat "has_hesitated" "VP" [Perf,Sg,Thrd] []]

lexicon "hurried" = [Cat "hurried" "VP" [Past] []]
lexicon "hurry" = [Cat "hurry" "VP" [Pres,Sg,Fst] [],
	Cat "hurry" "VP" [Pres,Sg,Snd] [],
	Cat "hurry" "VP" [Pres,Pl] [],
	Cat "hurry" "VP" [Infl] []]
lexicon "hurries" = [Cat "hurries" "VP" [Pres,Sg,Thrd] []]
lexicon "will_hurry" = [Cat "will_hurry" "VP" [Fut] []]
lexicon "have_hurried" = [Cat "have_hurried" "VP" [Perf,Sg,Fst] [],
	Cat "have_hurried" "VP" [Perf,Sg,Snd] [],
	Cat "have_hurried" "VP" [Perf,Pl] []]
lexicon "has_hurried" = [Cat "has_hurried" "VP" [Perf,Sg,Thrd] []]

lexicon "lied" = [Cat "lied" "VP" [Past] []]
lexicon "lie" = [Cat "lie" "VP" [Pres,Sg,Fst] [],
	Cat "lie" "VP" [Pres,Sg,Snd] [],
	Cat "lie" "VP" [Pres,Pl] [],
	Cat "lie" "VP" [Infl] []]
lexicon "lies" = [Cat "lies" "VP" [Pres,Sg,Thrd] []]
lexicon "will_lie" = [Cat "will_lie" "VP" [Fut] []]
lexicon "have_lied" = [Cat "have_lied" "VP" [Perf,Sg,Fst] [],
	Cat "have_lied" "VP" [Perf,Sg,Snd] [],
	Cat "have_lied" "VP" [Perf,Pl] []]
lexicon "has_lied" = [Cat "has_lied" "VP" [Perf,Sg,Thrd] []]

lexicon "meditated" = [Cat "meditated" "VP" [Past] []]
lexicon "meditate" = [Cat "meditate" "VP" [Pres,Sg,Fst] [],
	Cat "meditate" "VP" [Pres,Sg,Snd] [],
	Cat "meditate" "VP" [Pres,Pl] [],
	Cat "meditate" "VP" [Infl] []]
lexicon "meditates" = [Cat "meditates" "VP" [Pres,Sg,Thrd] []]
lexicon "will_meditate" = [Cat "will_meditate" "VP" [Fut] []]
lexicon "have_meditated" = [Cat "have_meditated" "VP" [Perf,Sg,Fst] [],
	Cat "have_meditated" "VP" [Perf,Sg,Snd] [],
	Cat "have_meditated" "VP" [Perf,Pl] []]
lexicon "has_meditated" = [Cat "has_meditated" "VP" [Perf,Sg,Thrd] []]

lexicon "occurred" = [Cat "occurred" "VP" [Past] []]
lexicon "occur" = [Cat "occur" "VP" [Pres,Sg,Fst] [],
	Cat "occur" "VP" [Pres,Sg,Snd] [],
	Cat "occur" "VP" [Pres,Pl] [],
	Cat "occur" "VP" [Infl] []]
lexicon "occurs" = [Cat "occurs" "VP" [Pres,Sg,Thrd] []]
lexicon "will_occur" = [Cat "will_occur" "VP" [Fut] []]
lexicon "have_occurred" = [Cat "have_occurred" "VP" [Perf,Sg,Fst] [],
	Cat "have_occurred" "VP" [Perf,Sg,Snd] [],
	Cat "have_occurred" "VP" [Perf,Pl] []]
lexicon "has_occurred" = [Cat "has_occurred" "VP" [Perf,Sg,Thrd] []]

lexicon "posed" = [Cat "posed" "VP" [Past] []]
lexicon "pose" = [Cat "pose" "VP" [Pres,Sg,Fst] [],
	Cat "pose" "VP" [Pres,Sg,Snd] [],
	Cat "pose" "VP" [Pres,Pl] [],
	Cat "pose" "VP" [Infl] []]
lexicon "poses" = [Cat "poses" "VP" [Pres,Sg,Thrd] []]
lexicon "will_pose" = [Cat "will_pose" "VP" [Fut] []]
lexicon "have_posed" = [Cat "have_posed" "VP" [Perf,Sg,Fst] [],
	Cat "have_posed" "VP" [Perf,Sg,Snd] [],
	Cat "have_posed" "VP" [Perf,Pl] []]
lexicon "has_posed" = [Cat "has_posed" "VP" [Perf,Sg,Thrd] []]

lexicon "prayed" = [Cat "prayed" "VP" [Past] []]
lexicon "pray" = [Cat "pray" "VP" [Pres,Sg,Fst] [],
	Cat "pray" "VP" [Pres,Sg,Snd] [],
	Cat "pray" "VP" [Pres,Pl] [],
	Cat "pray" "VP" [Infl] []]
lexicon "prays" = [Cat "prays" "VP" [Pres,Sg,Thrd] []]
lexicon "will_pray" = [Cat "will_pray" "VP" [Fut] []]
lexicon "have_prayed" = [Cat "have_prayed" "VP" [Perf,Sg,Fst] [],
	Cat "have_prayed" "VP" [Perf,Sg,Snd] [],
	Cat "have_prayed" "VP" [Perf,Pl] []]
lexicon "has_prayed" = [Cat "has_prayed" "VP" [Perf,Sg,Thrd] []]

lexicon "reacted" = [Cat "reacted" "VP" [Past] []]
lexicon "react" = [Cat "react" "VP" [Pres,Sg,Fst] [],
	Cat "react" "VP" [Pres,Sg,Snd] [],
	Cat "react" "VP" [Pres,Pl] [],
	Cat "react" "VP" [Infl] []]
lexicon "reacts" = [Cat "reacts" "VP" [Pres,Sg,Thrd] []]
lexicon "will_react" = [Cat "will_react" "VP" [Fut] []]
lexicon "have_reacted" = [Cat "have_reacted" "VP" [Perf,Sg,Fst] [],
	Cat "have_reacted" "VP" [Perf,Sg,Snd] [],
	Cat "have_reacted" "VP" [Perf,Pl] []]
lexicon "has_reacted" = [Cat "has_reacted" "VP" [Perf,Sg,Thrd] []]

lexicon "rebeled" = [Cat "rebeled" "VP" [Past] []]
lexicon "rebel" = [Cat "rebel" "VP" [Pres,Sg,Fst] [],
	Cat "rebel" "VP" [Pres,Sg,Snd] [],
	Cat "rebel" "VP" [Pres,Pl] [],
	Cat "rebel" "VP" [Infl] []]
lexicon "rebels" = [Cat "rebels" "VP" [Pres,Sg,Thrd] []]
lexicon "will_rebel" = [Cat "will_rebel" "VP" [Fut] []]
lexicon "have_rebeled" = [Cat "have_rebeled" "VP" [Perf,Sg,Fst] [],
	Cat "have_rebeled" "VP" [Perf,Sg,Snd] [],
	Cat "have_rebeled" "VP" [Perf,Pl] []]
lexicon "has_rebeled" = [Cat "has_rebeled" "VP" [Perf,Sg,Thrd] []]

lexicon "relaxed" = [Cat "relaxed" "VP" [Past] []]
lexicon "relax" = [Cat "relax" "VP" [Pres,Sg,Fst] [],
	Cat "relax" "VP" [Pres,Sg,Snd] [],
	Cat "relax" "VP" [Pres,Pl] [],
	Cat "relax" "VP" [Infl] []]
lexicon "relaxes" = [Cat "relaxes" "VP" [Pres,Sg,Thrd] []]
lexicon "will_relax" = [Cat "will_relax" "VP" [Fut] []]
lexicon "have_relaxed" = [Cat "have_relaxed" "VP" [Perf,Sg,Fst] [],
	Cat "have_relaxed" "VP" [Perf,Sg,Snd] [],
	Cat "have_relaxed" "VP" [Perf,Pl] []]
lexicon "has_relaxed" = [Cat "has_relaxed" "VP" [Perf,Sg,Thrd] []]

lexicon "rested" = [Cat "rested" "VP" [Past] []]
lexicon "rest" = [Cat "rest" "VP" [Pres,Sg,Fst] [],
	Cat "rest" "VP" [Pres,Sg,Snd] [],
	Cat "rest" "VP" [Pres,Pl] [],
	Cat "rest" "VP" [Infl] []]
lexicon "rests" = [Cat "rests" "VP" [Pres,Sg,Thrd] []]
lexicon "will_rest" = [Cat "will_rest" "VP" [Fut] []]
lexicon "have_rested" = [Cat "have_rested" "VP" [Perf,Sg,Fst] [],
	Cat "have_rested" "VP" [Perf,Sg,Snd] [],
	Cat "have_rested" "VP" [Perf,Pl] []]
lexicon "has_rested" = [Cat "has_rested" "VP" [Perf,Sg,Thrd] []]

lexicon "ran" = [Cat "ran" "VP" [Past] []]
lexicon "run" = [Cat "run" "VP" [Pres,Sg,Fst] [],
	Cat "run" "VP" [Pres,Sg,Snd] [],
	Cat "run" "VP" [Pres,Pl] [],
	Cat "run" "VP" [Infl] []]
lexicon "runs" = [Cat "runs" "VP" [Pres,Sg,Thrd] []]
lexicon "will_run" = [Cat "will_run" "VP" [Fut] []]
lexicon "have_run" = [Cat "have_run" "VP" [Perf,Sg,Fst] [],
	Cat "have_run" "VP" [Perf,Sg,Snd] [],
	Cat "have_run" "VP" [Perf,Pl] []]
lexicon "has_run" = [Cat "has_run" "VP" [Perf,Sg,Thrd] []]

lexicon "screamed" = [Cat "screamed" "VP" [Past] []]
lexicon "scream" = [Cat "scream" "VP" [Pres,Sg,Fst] [],
	Cat "scream" "VP" [Pres,Sg,Snd] [],
	Cat "scream" "VP" [Pres,Pl] [],
	Cat "scream" "VP" [Infl] []]
lexicon "screams" = [Cat "screams" "VP" [Pres,Sg,Thrd] []]
lexicon "will_scream" = [Cat "will_scream" "VP" [Fut] []]
lexicon "have_screamed" = [Cat "have_screamed" "VP" [Perf,Sg,Fst] [],
	Cat "have_screamed" "VP" [Perf,Sg,Snd] [],
	Cat "have_screamed" "VP" [Perf,Pl] []]
lexicon "has_screamed" = [Cat "has_screamed" "VP" [Perf,Sg,Thrd] []]

lexicon "shone" = [Cat "shone" "VP" [Past] []]
lexicon "shine" = [Cat "shine" "VP" [Pres,Sg,Fst] [],
	Cat "shine" "VP" [Pres,Sg,Snd] [],
	Cat "shine" "VP" [Pres,Pl] [],
	Cat "shine" "VP" [Infl] []]
lexicon "shines" = [Cat "shines" "VP" [Pres,Sg,Thrd] []]
lexicon "will_shine" = [Cat "will_shine" "VP" [Fut] []]
lexicon "have_shone" = [Cat "have_shone" "VP" [Perf,Sg,Fst] [],
	Cat "have_shone" "VP" [Perf,Sg,Snd] [],
	Cat "have_shone" "VP" [Perf,Pl] []]
lexicon "has_shone" = [Cat "has_shone" "VP" [Perf,Sg,Thrd] []]

lexicon "shrank" = [Cat "shrank" "VP" [Past] []]
lexicon "shrink" = [Cat "shrink" "VP" [Pres,Sg,Fst] [],
	Cat "shrink" "VP" [Pres,Sg,Snd] [],
	Cat "shrink" "VP" [Pres,Pl] [],
	Cat "shrink" "VP" [Infl] []]
lexicon "shrinks" = [Cat "shrinks" "VP" [Pres,Sg,Thrd] []]
lexicon "will_shrink" = [Cat "will_shrink" "VP" [Fut] []]
lexicon "have_shrunk" = [Cat "have_shrunk" "VP" [Perf,Sg,Fst] [],
	Cat "have_shrunk" "VP" [Perf,Sg,Snd] [],
	Cat "have_shrunk" "VP" [Perf,Pl] []]
lexicon "has_shrunk" = [Cat "has_shrunk" "VP" [Perf,Sg,Thrd] []]

lexicon "sank" = [Cat "sank" "VP" [Past] []]
lexicon "sink" = [Cat "sink" "VP" [Pres,Sg,Fst] [],
	Cat "sink" "VP" [Pres,Sg,Snd] [],
	Cat "sink" "VP" [Pres,Pl] [],
	Cat "sink" "VP" [Infl] []]
lexicon "sinks" = [Cat "sinks" "VP" [Pres,Sg,Thrd] []]
lexicon "will_sink" = [Cat "will_sink" "VP" [Fut] []]
lexicon "have_sunk" = [Cat "have_sunk" "VP" [Perf,Sg,Fst] [],
	Cat "have_sunk" "VP" [Perf,Sg,Snd] [],
	Cat "have_sunk" "VP" [Perf,Pl] []]
lexicon "has_sunk" = [Cat "has_sunk" "VP" [Perf,Sg,Thrd] []]

lexicon "sat" = [Cat "sat" "VP" [Past] []]
lexicon "sit" = [Cat "sit" "VP" [Pres,Sg,Fst] [],
	Cat "sit" "VP" [Pres,Sg,Snd] [],
	Cat "sit" "VP" [Pres,Pl] [],
	Cat "sit" "VP" [Infl] []]
lexicon "sits" = [Cat "sits" "VP" [Pres,Sg,Thrd] []]
lexicon "will_sit" = [Cat "will_sit" "VP" [Fut] []]
lexicon "have_sat" = [Cat "have_sat" "VP" [Perf,Sg,Fst] [],
	Cat "have_sat" "VP" [Perf,Sg,Snd] [],
	Cat "have_sat" "VP" [Perf,Pl] []]
lexicon "has_sat" = [Cat "has_sat" "VP" [Perf,Sg,Thrd] []]

lexicon "slept" = [Cat "slept" "VP" [Past] []]
lexicon "sleep" = [Cat "sleep" "VP" [Pres,Sg,Fst] [],
	Cat "sleep" "VP" [Pres,Sg,Snd] [],
	Cat "sleep" "VP" [Pres,Pl] [],
	Cat "sleep" "VP" [Infl] []]
lexicon "sleeps" = [Cat "sleeps" "VP" [Pres,Sg,Thrd] []]
lexicon "will_sleep" = [Cat "will_sleep" "VP" [Fut] []]
lexicon "have_slept" = [Cat "have_slept" "VP" [Perf,Sg,Fst] [],
	Cat "have_slept" "VP" [Perf,Sg,Snd] [],
	Cat "have_slept" "VP" [Perf,Pl] []]
lexicon "has_slept" = [Cat "has_slept" "VP" [Perf,Sg,Thrd] []]

lexicon "slipped" = [Cat "slipped" "VP" [Past] []]
lexicon "slip" = [Cat "slip" "VP" [Pres,Sg,Fst] [],
	Cat "slip" "VP" [Pres,Sg,Snd] [],
	Cat "slip" "VP" [Pres,Pl] [],
	Cat "slip" "VP" [Infl] []]
lexicon "slips" = [Cat "slips" "VP" [Pres,Sg,Thrd] []]
lexicon "will_slip" = [Cat "will_slip" "VP" [Fut] []]
lexicon "have_slipped" = [Cat "have_slipped" "VP" [Perf,Sg,Fst] [],
	Cat "have_slipped" "VP" [Perf,Sg,Snd] [],
	Cat "have_slipped" "VP" [Perf,Pl] []]
lexicon "has_slipped" = [Cat "has_slipped" "VP" [Perf,Sg,Thrd] []]

lexicon "snuck" = [Cat "snuck" "VP" [Past] []]
lexicon "sneak" = [Cat "sneak" "VP" [Pres,Sg,Fst] [],
	Cat "sneak" "VP" [Pres,Sg,Snd] [],
	Cat "sneak" "VP" [Pres,Pl] [],
	Cat "sneak" "VP" [Infl] []]
lexicon "sneaks" = [Cat "sneaks" "VP" [Pres,Sg,Thrd] []]
lexicon "will_sneak" = [Cat "will_sneak" "VP" [Fut] []]
lexicon "have_snuck" = [Cat "have_snuck" "VP" [Perf,Sg,Fst] [],
	Cat "have_snuck" "VP" [Perf,Sg,Snd] [],
	Cat "have_snuck" "VP" [Perf,Pl] []]
lexicon "has_snuck" = [Cat "has_snuck" "VP" [Perf,Sg,Thrd] []]

lexicon "snored" = [Cat "snored" "VP" [Past] []]
lexicon "snore" = [Cat "snore" "VP" [Pres,Sg,Fst] [],
	Cat "snore" "VP" [Pres,Sg,Snd] [],
	Cat "snore" "VP" [Pres,Pl] [],
	Cat "snore" "VP" [Infl] []]
lexicon "snores" = [Cat "snores" "VP" [Pres,Sg,Thrd] []]
lexicon "will_snore" = [Cat "will_snore" "VP" [Fut] []]
lexicon "have_snored" = [Cat "have_snored" "VP" [Perf,Sg,Fst] [],
	Cat "have_snored" "VP" [Perf,Sg,Snd] [],
	Cat "have_snored" "VP" [Perf,Pl] []]
lexicon "has_snored" = [Cat "has_snored" "VP" [Perf,Sg,Thrd] []]

lexicon "spoke" = [Cat "spoke" "VP" [Past] []]
lexicon "speak" = [Cat "speak" "VP" [Pres,Sg,Fst] [],
	Cat "speak" "VP" [Pres,Sg,Snd] [],
	Cat "speak" "VP" [Pres,Pl] [],
	Cat "speak" "VP" [Infl] []]
lexicon "speaks" = [Cat "speaks" "VP" [Pres,Sg,Thrd] []]
lexicon "will_speak" = [Cat "will_speak" "VP" [Fut] []]
lexicon "have_spoken" = [Cat "have_spoken" "VP" [Perf,Sg,Fst] [],
	Cat "have_spoken" "VP" [Perf,Sg,Snd] [],
	Cat "have_spoken" "VP" [Perf,Pl] []]
lexicon "has_spoken" = [Cat "has_spoken" "VP" [Perf,Sg,Thrd] []]

lexicon "spat" = [Cat "spat" "VP" [Past] []]
lexicon "spit" = [Cat "spit" "VP" [Pres,Sg,Fst] [],
	Cat "spit" "VP" [Pres,Sg,Snd] [],
	Cat "spit" "VP" [Pres,Pl] [],
	Cat "spit" "VP" [Infl] []]
lexicon "spits" = [Cat "spits" "VP" [Pres,Sg,Thrd] []]
lexicon "will_spit" = [Cat "will_spit" "VP" [Fut] []]
lexicon "have_spat" = [Cat "have_spat" "VP" [Perf,Sg,Fst] [],
	Cat "have_spat" "VP" [Perf,Sg,Snd] [],
	Cat "have_spat" "VP" [Perf,Pl] []]
lexicon "has_spat" = [Cat "has_spat" "VP" [Perf,Sg,Thrd] []]

lexicon "squeaked" = [Cat "squeaked" "VP" [Past] []]
lexicon "squeak" = [Cat "squeak" "VP" [Pres,Sg,Fst] [],
	Cat "squeak" "VP" [Pres,Sg,Snd] [],
	Cat "squeak" "VP" [Pres,Pl] [],
	Cat "squeak" "VP" [Infl] []]
lexicon "squeaks" = [Cat "squeaks" "VP" [Pres,Sg,Thrd] []]
lexicon "will_squeak" = [Cat "will_squeak" "VP" [Fut] []]
lexicon "have_squeaked" = [Cat "have_squeaked" "VP" [Perf,Sg,Fst] [],
	Cat "have_squeaked" "VP" [Perf,Sg,Snd] [],
	Cat "have_squeaked" "VP" [Perf,Pl] []]
lexicon "has_squeaked" = [Cat "has_squeaked" "VP" [Perf,Sg,Thrd] []]

lexicon "stank" = [Cat "stank" "VP" [Past] []]
lexicon "stink" = [Cat "stink" "VP" [Pres,Sg,Fst] [],
	Cat "stink" "VP" [Pres,Sg,Snd] [],
	Cat "stink" "VP" [Pres,Pl] [],
	Cat "stink" "VP" [Infl] []]
lexicon "stinks" = [Cat "stinks" "VP" [Pres,Sg,Thrd] []]
lexicon "will_stink" = [Cat "will_stink" "VP" [Fut] []]
lexicon "have_stunk" = [Cat "have_stunk" "VP" [Perf,Sg,Fst] [],
	Cat "have_stunk" "VP" [Perf,Sg,Snd] [],
	Cat "have_stunk" "VP" [Perf,Pl] []]
lexicon "has_stunk" = [Cat "has_stunk" "VP" [Perf,Sg,Thrd] []]

lexicon "sweated" = [Cat "sweated" "VP" [Past] []]
lexicon "sweat" = [Cat "sweat" "VP" [Pres,Sg,Fst] [],
	Cat "sweat" "VP" [Pres,Sg,Snd] [],
	Cat "sweat" "VP" [Pres,Pl] [],
	Cat "sweat" "VP" [Infl] []]
lexicon "sweats" = [Cat "sweats" "VP" [Pres,Sg,Thrd] []]
lexicon "will_sweat" = [Cat "will_sweat" "VP" [Fut] []]
lexicon "have_sweated" = [Cat "have_sweated" "VP" [Perf,Sg,Fst] [],
	Cat "have_sweated" "VP" [Perf,Sg,Snd] [],
	Cat "have_sweated" "VP" [Perf,Pl] []]
lexicon "has_sweated" = [Cat "has_sweated" "VP" [Perf,Sg,Thrd] []]

lexicon "froze" = [Cat "froze" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "freeze" = [Cat "freeze" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "freeze" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "freeze" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
	Cat "freeze" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "freezes" = [Cat "freezes" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_freeze" = [Cat "will_freeze" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_frozen" = [Cat "have_frozen" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_frozen" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_frozen" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_frozen" = [Cat "has_frozen" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "abandoned" = [Cat "abandoned" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "abandon" = [Cat "abandon" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "abandon" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "abandon" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
	Cat "abandon" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "abandons" = [Cat "abandons" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_abandon" = [Cat "will_abandon" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_abandoned" = [Cat "have_abandoned" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_abandoned" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_abandoned" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_abandoned" = [Cat "has_abandoned" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "abhorred" = [Cat "abhorred" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "abhor" = [Cat "abhor" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "abhor" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "abhor" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
	Cat "abhor" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "abhors" = [Cat "abhors" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_abhor" = [Cat "will_abhor" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_abhorred" = [Cat "have_abhorred" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_abhorred" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_abhorred" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_abhorred" = [Cat "has_abhorred" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "avoided" = [Cat "avoided" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "avoid" = [Cat "avoid" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "avoid" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "avoid" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
	Cat "avoid" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "avoids" = [Cat "avoids" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_avoid" = [Cat "will_avoid" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_avoided" = [Cat "have_avoided" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_avoided" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_avoided" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_avoided" = [Cat "has_avoided" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "absorbed" = [Cat "absorbed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "absorb" = [Cat "absorb" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "absorb" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "absorb" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
	Cat "absorb" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "absorbs" = [Cat "absorbs" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_absorb" = [Cat "will_absorb" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_absorbed" = [Cat "have_absorbed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_absorbed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_absorbed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_absorbed" = [Cat "has_absorbed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "abused" = [Cat "abused" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "abuse" = [Cat "abuse" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "abuse" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "abuse" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
	Cat "abuse" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "abuses" = [Cat "abuses" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_abuse" = [Cat "will_abuse" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_abused" = [Cat "have_abused" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_abused" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_abused" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_abused" = [Cat "has_abused" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "accomodated" = [Cat "accomodated" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "accomodate" = [Cat "accomodate" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "accomodate" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "accomodate" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
	Cat "accomodate" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "accomodates" = [Cat "accomodates" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_accomodate" = [Cat "will_accomodate" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_accomodated" = [Cat "have_accomodated" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_accomodated" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_accomodated" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_accomodated" = [Cat "has_accomodated" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "accompanied" = [Cat "accompanied" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "accompany" = [Cat "accompany" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "accompany" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "accompany" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
	Cat "accompany" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "accompanies" = [Cat "accompanies" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_accompany" = [Cat "will_accompany" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_accompanied" = [Cat "have_accompanied" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_accompanied" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_accompanied" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_accompanied" = [Cat "has_accompanied" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "achieved" = [Cat "achieved" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "achieve" = [Cat "achieve" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "achieve" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "achieve" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
	Cat "achieve" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "achieves" = [Cat "achieves" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_achieve" = [Cat "will_achieve" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_achieved" = [Cat "have_achieved" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_achieved" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_achieved" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_achieved" = [Cat "has_achieved" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "acquired" = [Cat "acquired" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "acquire" = [Cat "acquire" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "acquire" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "acquire" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
	Cat "acquire" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "acquires" = [Cat "acquires" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_acquire" = [Cat "will_acquire" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_acquired" = [Cat "have_acquired" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_acquired" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_acquired" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_acquired" = [Cat "has_acquired" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "adopted" = [Cat "adopted" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "adopt" = [Cat "adopt" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "adopt" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "adopt" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
	Cat "adopt" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "adopts" = [Cat "adopts" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_adopt" = [Cat "will_adopt" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_adopted" = [Cat "have_adopted" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_adopted" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_adopted" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_adopted" = [Cat "has_adopted" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "aided" = [Cat "aided" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "aid" = [Cat "aid" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "aid" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "aid" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
	Cat "aid" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "aids" = [Cat "aids" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_aid" = [Cat "will_aid" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_aided" = [Cat "have_aided" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_aided" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_aided" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_aided" = [Cat "has_aided" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "alarmed" = [Cat "alarmed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "alarm" = [Cat "alarm" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "alarm" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "alarm" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
	Cat "alarm" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "alarms" = [Cat "alarms" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_alarm" = [Cat "will_alarm" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_alarmed" = [Cat "have_alarmed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_alarmed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_alarmed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_alarmed" = [Cat "has_alarmed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "amazed" = [Cat "amazed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "amaze" = [Cat "amaze" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "amaze" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "amaze" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
	Cat "amaze" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "amazes" = [Cat "amazes" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_amaze" = [Cat "will_amaze" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_amazed" = [Cat "have_amazed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_amazed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_amazed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_amazed" = [Cat "has_amazed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "amused" = [Cat "amused" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "amuse" = [Cat "amuse" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "amuse" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "amuse" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
	Cat "amuse" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "amuses" = [Cat "amuses" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_amuse" = [Cat "will_amuse" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_amused" = [Cat "have_amused" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_amused" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_amused" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_amused" = [Cat "has_amused" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "annoyed" = [Cat "annoyed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "annoy" = [Cat "annoy" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "annoy" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "annoy" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
	Cat "annoy" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "annoys" = [Cat "annoys" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_annoy" = [Cat "will_annoy" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_annoyed" = [Cat "have_annoyed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_annoyed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_annoyed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_annoyed" = [Cat "has_annoyed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "approached" = [Cat "approached" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "approach" = [Cat "approach" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "approach" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "approach" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
	Cat "approach" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "approaches" = [Cat "approaches" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_approach" = [Cat "will_approach" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_approached" = [Cat "have_approached" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_approached" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_approached" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_approached" = [Cat "has_approached" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "astounded" = [Cat "astounded" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "astound" = [Cat "astound" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "astound" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "astound" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
	Cat "astound" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "astounds" = [Cat "astounds" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_astound" = [Cat "will_astound" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_astounded" = [Cat "have_astounded" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_astounded" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_astounded" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_astounded" = [Cat "has_astounded" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "awaited" = [Cat "awaited" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "await" = [Cat "await" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "await" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "await" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
	Cat "await" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "awaits" = [Cat "awaits" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_await" = [Cat "will_await" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_awaited" = [Cat "have_awaited" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_awaited" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_awaited" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_awaited" = [Cat "has_awaited" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "banned" = [Cat "banned" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "ban" = [Cat "ban" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "ban" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "ban" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
	Cat "ban" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "bans" = [Cat "bans" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_ban" = [Cat "will_ban" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_banned" = [Cat "have_banned" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_banned" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_banned" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_banned" = [Cat "has_banned" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "became" = [Cat "became" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "become" = [Cat "become" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "become" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "become" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
	Cat "become" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "becomes" = [Cat "becomes" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_become" = [Cat "will_become" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_become" = [Cat "have_become" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_become" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_become" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_become" = [Cat "has_become" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "believed" = [Cat "believed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "believe" = [Cat "believe" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "believe" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "believe" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
	Cat "believe" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "believes" = [Cat "believes" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_believe" = [Cat "will_believe" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_believed" = [Cat "have_believed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_believed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_believed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_believed" = [Cat "has_believed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "broke" = [Cat "broke" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "break" = [Cat "break" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "break" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "break" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
	Cat "break" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "breaks" = [Cat "breaks" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_break" = [Cat "will_break" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_broken" = [Cat "have_broken" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_broken" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_broken" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_broken" = [Cat "has_broken" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "captured" = [Cat "captured" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "capture" = [Cat "capture" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "capture" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "capture" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
	Cat "capture" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "captures" = [Cat "captures" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_capture" = [Cat "will_capture" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_captured" = [Cat "have_captured" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_captured" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_captured" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_captured" = [Cat "has_captured" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "chased" = [Cat "chased" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "chase" = [Cat "chase" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "chase" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "chase" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
	Cat "chase" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "chases" = [Cat "chases" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_chase" = [Cat "will_chase" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_chased" = [Cat "have_chased" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_chased" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_chased" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_chased" = [Cat "has_chased" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "chewed" = [Cat "chewed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "chew" = [Cat "chew" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "chew" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "chew" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
	Cat "chew" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "chews" = [Cat "chews" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_chew" = [Cat "will_chew" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_chewed" = [Cat "have_chewed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_chewed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_chewed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_chewed" = [Cat "has_chewed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "chopped" = [Cat "chopped" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "chop" = [Cat "chop" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "chop" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "chop" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
	Cat "chop" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "chops" = [Cat "chops" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_chop" = [Cat "will_chop" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_chopped" = [Cat "have_chopped" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_chopped" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_chopped" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_chopped" = [Cat "has_chopped" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "devoured" = [Cat "devoured" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "devour" = [Cat "devour" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "devour" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "devour" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
	Cat "devour" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "devours" = [Cat "devours" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_devour" = [Cat "will_devour" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_devoured" = [Cat "have_devoured" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_devoured" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_devoured" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_devoured" = [Cat "has_devoured" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "disciplined" = [Cat "disciplined" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "discipline" = [Cat "discipline" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "discipline" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "discipline" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
	Cat "discipline" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "disciplines" = [Cat "disciplines" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_discipline" = [Cat "will_discipline" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_disciplined" = [Cat "have_disciplined" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_disciplined" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_disciplined" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_disciplined" = [Cat "has_disciplined" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "disobeyed" = [Cat "disobeyed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "disobey" = [Cat "disobey" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "disobey" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "disobey" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
	Cat "disobey" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "disobeys" = [Cat "disobeys" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_disobey" = [Cat "will_disobey" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_disobeyed" = [Cat "have_disobeyed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_disobeyed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_disobeyed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_disobeyed" = [Cat "has_disobeyed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "found" = [Cat "found" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "find" = [Cat "find" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "find" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "find" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
	Cat "find" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "finds" = [Cat "finds" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_find" = [Cat "will_find" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_found" = [Cat "have_found" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_found" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_found" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_found" = [Cat "has_found" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "flattered" = [Cat "flattered" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "flatter" = [Cat "flatter" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "flatter" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "flatter" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
	Cat "flatter" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "flatters" = [Cat "flatters" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_flatter" = [Cat "will_flatter" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_flattered" = [Cat "have_flattered" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_flattered" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_flattered" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_flattered" = [Cat "has_flattered" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "followed" = [Cat "followed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "follow" = [Cat "follow" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "follow" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "follow" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
	Cat "follow" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "follows" = [Cat "follows" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_follow" = [Cat "will_follow" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_followed" = [Cat "have_followed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_followed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_followed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_followed" = [Cat "has_followed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "handled" = [Cat "handled" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "handle" = [Cat "handle" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "handle" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "handle" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
	Cat "handle" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "handles" = [Cat "handles" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_handle" = [Cat "will_handle" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_handled" = [Cat "have_handled" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_handled" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_handled" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_handled" = [Cat "has_handled" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "heard" = [Cat "heard" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "hear" = [Cat "hear" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "hear" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "hear" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
	Cat "hear" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "hears" = [Cat "hears" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_hear" = [Cat "will_hear" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_heard" = [Cat "have_heard" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_heard" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_heard" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_heard" = [Cat "has_heard" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "helped" = [Cat "helped" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "help" = [Cat "help" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "help" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "help" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
	Cat "help" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "helps" = [Cat "helps" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_help" = [Cat "will_help" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_helped" = [Cat "have_helped" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_helped" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_helped" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_helped" = [Cat "has_helped" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "imitated" = [Cat "imitated" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "imitate" = [Cat "imitate" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "imitate" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "imitate" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
	Cat "imitate" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "imitates" = [Cat "imitates" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_imitate" = [Cat "will_imitate" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_imitated" = [Cat "have_imitated" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_imitated" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_imitated" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_imitated" = [Cat "has_imitated" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "imported" = [Cat "imported" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "import" = [Cat "import" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "import" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "import" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
	Cat "import" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "imports" = [Cat "imports" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_import" = [Cat "will_import" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_imported" = [Cat "have_imported" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_imported" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_imported" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_imported" = [Cat "has_imported" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "included" = [Cat "included" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "include" = [Cat "include" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "include" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "include" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
	Cat "include" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "includes" = [Cat "includes" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_include" = [Cat "will_include" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_included" = [Cat "have_included" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_included" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_included" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_included" = [Cat "has_included" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "knew" = [Cat "knew" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "know" = [Cat "know" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "know" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "know" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
	Cat "know" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "knows" = [Cat "knows" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_know" = [Cat "will_know" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_known" = [Cat "have_known" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_known" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_known" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_known" = [Cat "has_known" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "measured" = [Cat "measured" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "measure" = [Cat "measure" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "measure" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "measure" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
	Cat "measure" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "measures" = [Cat "measures" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_measure" = [Cat "will_measure" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_measured" = [Cat "have_measured" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_measured" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_measured" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_measured" = [Cat "has_measured" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "missed" = [Cat "missed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "miss" = [Cat "miss" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "miss" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "miss" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
	Cat "miss" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "misses" = [Cat "misses" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_miss" = [Cat "will_miss" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_missed" = [Cat "have_missed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_missed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_missed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_missed" = [Cat "has_missed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "nudged" = [Cat "nudged" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "nudge" = [Cat "nudge" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "nudge" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "nudge" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
	Cat "nudge" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "nudges" = [Cat "nudges" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_nudge" = [Cat "will_nudge" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_nudged" = [Cat "have_nudged" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_nudged" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_nudged" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_nudged" = [Cat "has_nudged" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "obtained" = [Cat "obtained" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "obtain" = [Cat "obtain" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "obtain" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "obtain" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
	Cat "obtain" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "obtains" = [Cat "obtains" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_obtain" = [Cat "will_obtain" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_obtained" = [Cat "have_obtained" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_obtained" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_obtained" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_obtained" = [Cat "has_obtained" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "owned" = [Cat "owned" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "own" = [Cat "own" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "own" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "own" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
	Cat "own" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "owns" = [Cat "owns" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_own" = [Cat "will_own" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_owned" = [Cat "have_owned" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_owned" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_owned" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_owned" = [Cat "has_owned" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "ate" = [Cat "ate" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "eat" = [Cat "eat" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "eat" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "eat" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
	Cat "eat" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "eats" = [Cat "eats" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_eat" = [Cat "will_eat" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_eaten" = [Cat "have_eaten" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_eaten" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_eaten" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_eaten" = [Cat "has_eaten" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "pitied" = [Cat "pitied" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "pity" = [Cat "pity" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "pity" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "pity" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
	Cat "pity" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "pities" = [Cat "pities" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_pity" = [Cat "will_pity" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_pitied" = [Cat "have_pitied" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_pitied" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_pitied" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_pitied" = [Cat "has_pitied" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "preserved" = [Cat "preserved" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "preserve" = [Cat "preserve" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "preserve" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "preserve" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
	Cat "preserve" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "preserves" = [Cat "preserves" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_preserve" = [Cat "will_preserve" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_preserved" = [Cat "have_preserved" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_preserved" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_preserved" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_preserved" = [Cat "has_preserved" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "pushed" = [Cat "pushed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "push" = [Cat "push" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "push" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "push" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
	Cat "push" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "pushes" = [Cat "pushes" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_push" = [Cat "will_push" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_pushed" = [Cat "have_pushed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_pushed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_pushed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_pushed" = [Cat "has_pushed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "raced" = [Cat "raced" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "race" = [Cat "race" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "race" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "race" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
	Cat "race" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "races" = [Cat "races" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_race" = [Cat "will_race" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_raced" = [Cat "have_raced" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_raced" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_raced" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_raced" = [Cat "has_raced" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "received" = [Cat "received" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "received" = [Cat "received" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "received" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "received" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
	Cat "received" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "receives" = [Cat "receives" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_received" = [Cat "will_received" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_received" = [Cat "have_received" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_received" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_received" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_received" = [Cat "has_received" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "rejected" = [Cat "rejected" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "reject" = [Cat "reject" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "reject" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "reject" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
	Cat "reject" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "rejects" = [Cat "rejects" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_reject" = [Cat "will_reject" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_rejected" = [Cat "have_rejected" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_rejected" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_rejected" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_rejected" = [Cat "has_rejected" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "remembered" = [Cat "remembered" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "remember" = [Cat "remember" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "remember" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "remember" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
	Cat "remember" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "remembers" = [Cat "remembers" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_remember" = [Cat "will_remember" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_remembered" = [Cat "have_remembered" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_remembered" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_remembered" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_remembered" = [Cat "has_remembered" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "killed" = [Cat "killed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "kill" = [Cat "kill" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "kill" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "kill" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
	Cat "kill" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "kills" = [Cat "kills" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_kill" = [Cat "will_kill" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_killed" = [Cat "have_killed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_killed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_killed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_killed" = [Cat "has_killed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "rode" = [Cat "rode" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "ride" = [Cat "ride" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "ride" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "ride" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
	Cat "ride" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "rides" = [Cat "rides" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_ride" = [Cat "will_ride" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_ridden" = [Cat "have_ridden" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_ridden" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_ridden" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_ridden" = [Cat "has_ridden" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "sought" = [Cat "sought" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "seek" = [Cat "seek" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "seek" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "seek" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
	Cat "seek" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "seeks" = [Cat "seeks" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_seek" = [Cat "will_seek" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_sought" = [Cat "have_sought" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_sought" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_sought" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_sought" = [Cat "has_sought" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "seized" = [Cat "seized" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "seize" = [Cat "seize" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "seize" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "seize" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
	Cat "seize" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "seizes" = [Cat "seizes" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_seize" = [Cat "will_seize" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_seized" = [Cat "have_seized" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_seized" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_seized" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_seized" = [Cat "has_seized" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "shaved" = [Cat "shaved" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "shave" = [Cat "shave" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "shave" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "shave" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
	Cat "shave" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "shaves" = [Cat "shaves" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_shave" = [Cat "will_shave" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_shaved" = [Cat "have_shaved" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_shaved" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_shaved" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_shaved" = [Cat "has_shaved" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "stung" = [Cat "stung" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "sting" = [Cat "sting" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "sting" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "sting" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
	Cat "sting" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "stings" = [Cat "stings" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_sting" = [Cat "will_sting" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_stung" = [Cat "have_stung" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_stung" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_stung" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_stung" = [Cat "has_stung" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "tested" = [Cat "tested" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "test" = [Cat "test" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "test" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "test" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
	Cat "test" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "tests" = [Cat "tests" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_test" = [Cat "will_test" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_tested" = [Cat "have_tested" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_tested" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
	Cat "have_tested" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_tested" = [Cat "has_tested" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon _ = []



conjToBase = [("shout","shout"),("shouted","shout"),("shouts","shout"),("will_shout","shout"),("has_shouted","shout"),("have_shouted","shout"),("aged","age"),("age","age"),("ages","age"),("will_age","age"),("have_aged","age"),("has_aged","age"),("arrived","arrive"),("arrive","arrive"),("arrives","arrive"),("will_arrive","arrive"),("have_arrived","arrive"),("has_arrived","arrive"),("ached","ache"),("ache","ache"),("aches","ache"),("will_ache","ache"),("have_ached","ache"),("has_ached","ache"),("advanced","advance"),("advance","advance"),("advances","advance"),("will_advance","advance"),("have_advanced","advance"),("has_advanced","advance"),("ailed","ail"),("ail","ail"),("ails","ail"),("will_ail","ail"),("have_ailed","ail"),("has_ailed","ail"),("apologized","apologize"),("apologize","apologize"),("apologizes","apologize"),("will_apologize","apologize"),("have_apologized","apologize"),("has_apologized","apologize"),("boasted","boast"),("boast","boast"),("boasts","boast"),("will_boast","boast"),("have_boasted","boast"),("has_boasted","boast"),("breathed","breathe"),("breathe","breathe"),("breathes","breathe"),("will_breathe","breathe"),("have_breathed","breathe"),("has_breathed","breathe"),("breeded","breed"),("breed","breed"),("breeds","breed"),("will_breed","breed"),("have_breeded","breed"),("has_breeded","breed"),("chanted","chant"),("chant","chant"),("chants","chant"),("will_chant","chant"),("have_chanted","chant"),("has_chanted","chant"),("cheated","cheat"),("cheat","cheat"),("cheats","cheat"),("will_cheat","cheat"),("have_cheated","cheat"),("has_cheated","cheat"),("came","come"),("come","come"),("comes","come"),("will_come","come"),("have_come","come"),("has_come","come"),("cried","cry"),("cry","cry"),("cries","cry"),("will_cry","cry"),("have_cried","cry"),("has_cried","cry"),("danced","dance"),("dance","dance"),("dances","dance"),("will_dance","dance"),("have_danced","dance"),("has_danced","dance"),("decayed","decay"),("decay","decay"),("decays","decay"),("will_decay","decay"),("have_decayed","decay"),("has_decayed","decay"),("delayed","delay"),("delay","delay"),("delays","delay"),("will_delay","delay"),("have_delayed","delay"),("has_delayed","delay"),("deliberated","deliberate"),("deliberate","deliberate"),("deliberates","deliberate"),("will_deliberate","deliberate"),("have_deliberated","deliberate"),("has_deliberated","deliberate"),("depreciated","depreciate"),("depreciate","depreciate"),("depreciates","depreciate"),("will_depreciate","depreciate"),("have_depreciated","depreciate"),("has_depreciated","depreciate"),("entered","enter"),("enter","enter"),("enters","enter"),("will_enter","enter"),("have_entered","enter"),("has_entered","enter"),("escaped","escape"),("escape","escape"),("escapes","escape"),("will_escape","escape"),("have_escaped","escape"),("has_escaped","escape"),("exhaled","exhale"),("exhale","exhale"),("exhales","exhale"),("will_exhale","exhale"),("have_exhaled","exhale"),("has_exhaled","exhale"),("exploded","explode"),("explode","explode"),("explodes","explode"),("will_explode","explode"),("have_exploded","explode"),("has_exploded","explode"),("failed","fail"),("fail","fail"),("fails","fail"),("will_fail","fail"),("have_failed","fail"),("has_failed","fail"),("flew","fly"),("fly","fly"),("flies","fly"),("will_fly","fly"),("have_flown","fly"),("has_flown","fly"),("functioned","function"),("function","function"),("functions","function"),("will_function","function"),("have_functioned","function"),("has_functioned","function"),("gambled","gamble"),("gamble","gamble"),("gambles","gamble"),("will_gamble","gamble"),("have_gambled","gamble"),("has_gambled","gamble"),("happened","happen"),("happen","happen"),("happens","happen"),("will_happen","happen"),("have_happened","happen"),("has_happened","happen"),("hesitated","hesitate"),("hesitate","hesitate"),("hesitates","hesitate"),("will_hesitate","hesitate"),("have_hesitated","hesitate"),("has_hesitated","hesitate"),("hurried","hurry"),("hurry","hurry"),("hurries","hurry"),("will_hurry","hurry"),("have_hurried","hurry"),("has_hurried","hurry"),("lied","lie"),("lie","lie"),("lies","lie"),("will_lie","lie"),("have_lied","lie"),("has_lied","lie"),("meditated","meditate"),("meditate","meditate"),("meditates","meditate"),("will_meditate","meditate"),("have_meditated","meditate"),("has_meditated","meditate"),("occurred","occur"),("occur","occur"),("occurs","occur"),("will_occur","occur"),("have_occurred","occur"),("has_occurred","occur"),("posed","pose"),("pose","pose"),("poses","pose"),("will_pose","pose"),("have_posed","pose"),("has_posed","pose"),("prayed","pray"),("pray","pray"),("prays","pray"),("will_pray","pray"),("have_prayed","pray"),("has_prayed","pray"),("reacted","react"),("react","react"),("reacts","react"),("will_react","react"),("have_reacted","react"),("has_reacted","react"),("rebeled","rebel"),("rebel","rebel"),("rebels","rebel"),("will_rebel","rebel"),("have_rebeled","rebel"),("has_rebeled","rebel"),("relaxed","relax"),("relax","relax"),("relaxes","relax"),("will_relax","relax"),("have_relaxed","relax"),("has_relaxed","relax"),("rested","rest"),("rest","rest"),("rests","rest"),("will_rest","rest"),("have_rested","rest"),("has_rested","rest"),("ran","run"),("run","run"),("runs","run"),("will_run","run"),("have_run","run"),("has_run","run"),("screamed","scream"),("scream","scream"),("screams","scream"),("will_scream","scream"),("have_screamed","scream"),("has_screamed","scream"),("shone","shine"),("shine","shine"),("shines","shine"),("will_shine","shine"),("have_shone","shine"),("has_shone","shine"),("shrank","shrink"),("shrink","shrink"),("shrinks","shrink"),("will_shrink","shrink"),("have_shrunk","shrink"),("has_shrunk","shrink"),("sank","sink"),("sink","sink"),("sinks","sink"),("will_sink","sink"),("have_sunk","sink"),("has_sunk","sink"),("sat","sit"),("sit","sit"),("sits","sit"),("will_sit","sit"),("have_sat","sit"),("has_sat","sit"),("slept","sleep"),("sleep","sleep"),("sleeps","sleep"),("will_sleep","sleep"),("have_slept","sleep"),("has_slept","sleep"),("slipped","slip"),("slip","slip"),("slips","slip"),("will_slip","slip"),("have_slipped","slip"),("has_slipped","slip"),("snuck","sneak"),("sneak","sneak"),("sneaks","sneak"),("will_sneak","sneak"),("have_snuck","sneak"),("has_snuck","sneak"),("snored","snore"),("snore","snore"),("snores","snore"),("will_snore","snore"),("have_snored","snore"),("has_snored","snore"),("spoke","speak"),("speak","speak"),("speaks","speak"),("will_speak","speak"),("have_spoken","speak"),("has_spoken","speak"),("spat","spit"),("spit","spit"),("spits","spit"),("will_spit","spit"),("have_spat","spit"),("has_spat","spit"),("squeaked","squeak"),("squeak","squeak"),("squeaks","squeak"),("will_squeak","squeak"),("have_squeaked","squeak"),("has_squeaked","squeak"),("stank","stink"),("stink","stink"),("stinks","stink"),("will_stink","stink"),("have_stunk","stink"),("has_stunk","stink"),("sweated","sweat"),("sweat","sweat"),("sweats","sweat"),("will_sweat","sweat"),("have_sweated","sweat"),("has_sweated","sweat"),("froze","freeze"),("freeze","freeze"),("freezes","freeze"),("will_freeze","freeze"),("have_frozen","freeze"),("has_frozen","freeze"),("abandoned","abandon"),("abandon","abandon"),("abandons","abandon"),("will_abandon","abandon"),("have_abandoned","abandon"),("has_abandoned","abandon"),("abhorred","abhor"),("abhor","abhor"),("abhors","abhor"),("will_abhor","abhor"),("have_abhorred","abhor"),("has_abhorred","abhor"),("avoided","avoid"),("avoid","avoid"),("avoids","avoid"),("will_avoid","avoid"),("have_avoided","avoid"),("has_avoided","avoid"),("absorbed","absorb"),("absorb","absorb"),("absorbs","absorb"),("will_absorb","absorb"),("have_absorbed","absorb"),("has_absorbed","absorb"),("abused","abuse"),("abuse","abuse"),("abuses","abuse"),("will_abuse","abuse"),("have_abused","abuse"),("has_abused","abuse"),("accomodated","accomodate"),("accomodate","accomodate"),("accomodates","accomodate"),("will_accomodate","accomodate"),("have_accomodated","accomodate"),("has_accomodated","accomodate"),("accompanied","accompany"),("accompany","accompany"),("accompanies","accompany"),("will_accompany","accompany"),("have_accompanied","accompany"),("has_accompanied","accompany"),("achieved","achieve"),("achieve","achieve"),("achieves","achieve"),("will_achieve","achieve"),("have_achieved","achieve"),("has_achieved","achieve"),("acquired","acquire"),("acquire","acquire"),("acquires","acquire"),("will_acquire","acquire"),("have_acquired","acquire"),("has_acquired","acquire"),("adopted","adopt"),("adopt","adopt"),("adopts","adopt"),("will_adopt","adopt"),("have_adopted","adopt"),("has_adopted","adopt"),("aided","aid"),("aid","aid"),("aids","aid"),("will_aid","aid"),("have_aided","aid"),("has_aided","aid"),("alarmed","alarm"),("alarm","alarm"),("alarms","alarm"),("will_alarm","alarm"),("have_alarmed","alarm"),("has_alarmed","alarm"),("amazed","amaze"),("amaze","amaze"),("amazes","amaze"),("will_amaze","amaze"),("have_amazed","amaze"),("has_amazed","amaze"),("amused","amuse"),("amuse","amuse"),("amuses","amuse"),("will_amuse","amuse"),("have_amused","amuse"),("has_amused","amuse"),("annoyed","annoy"),("annoy","annoy"),("annoys","annoy"),("will_annoy","annoy"),("have_annoyed","annoy"),("has_annoyed","annoy"),("approached","approach"),("approach","approach"),("approaches","approach"),("will_approach","approach"),("have_approached","approach"),("has_approached","approach"),("astounded","astound"),("astound","astound"),("astounds","astound"),("will_astound","astound"),("have_astounded","astound"),("has_astounded","astound"),("awaited","await"),("await","await"),("awaits","await"),("will_await","await"),("have_awaited","await"),("has_awaited","await"),("banned","ban"),("ban","ban"),("bans","ban"),("will_ban","ban"),("have_banned","ban"),("has_banned","ban"),("became","become"),("become","become"),("becomes","become"),("will_become","become"),("have_become","become"),("has_become","become"),("believed","believe"),("believe","believe"),("believes","believe"),("will_believe","believe"),("have_believed","believe"),("has_believed","believe"),("broke","break"),("break","break"),("breaks","break"),("will_break","break"),("have_broken","break"),("has_broken","break"),("captured","capture"),("capture","capture"),("captures","capture"),("will_capture","capture"),("have_captured","capture"),("has_captured","capture"),("chased","chase"),("chase","chase"),("chases","chase"),("will_chase","chase"),("have_chased","chase"),("has_chased","chase"),("chewed","chew"),("chew","chew"),("chews","chew"),("will_chew","chew"),("have_chewed","chew"),("has_chewed","chew"),("chopped","chop"),("chop","chop"),("chops","chop"),("will_chop","chop"),("have_chopped","chop"),("has_chopped","chop"),("devoured","devour"),("devour","devour"),("devours","devour"),("will_devour","devour"),("have_devoured","devour"),("has_devoured","devour"),("disciplined","discipline"),("discipline","discipline"),("disciplines","discipline"),("will_discipline","discipline"),("have_disciplined","discipline"),("has_disciplined","discipline"),("disobeyed","disobey"),("disobey","disobey"),("disobeys","disobey"),("will_disobey","disobey"),("have_disobeyed","disobey"),("has_disobeyed","disobey"),("found","find"),("find","find"),("finds","find"),("will_find","find"),("have_found","find"),("has_found","find"),("flattered","flatter"),("flatter","flatter"),("flatters","flatter"),("will_flatter","flatter"),("have_flattered","flatter"),("has_flattered","flatter"),("followed","follow"),("follow","follow"),("follows","follow"),("will_follow","follow"),("have_followed","follow"),("has_followed","follow"),("handled","handle"),("handle","handle"),("handles","handle"),("will_handle","handle"),("have_handled","handle"),("has_handled","handle"),("heard","hear"),("hear","hear"),("hears","hear"),("will_hear","hear"),("have_heard","hear"),("has_heard","hear"),("helped","help"),("help","help"),("helps","help"),("will_help","help"),("have_helped","help"),("has_helped","help"),("imitated","imitate"),("imitate","imitate"),("imitates","imitate"),("will_imitate","imitate"),("have_imitated","imitate"),("has_imitated","imitate"),("imported","import"),("import","import"),("imports","import"),("will_import","import"),("have_imported","import"),("has_imported","import"),("included","include"),("include","include"),("includes","include"),("will_include","include"),("have_included","include"),("has_included","include"),("knew","know"),("know","know"),("knows","know"),("will_know","know"),("have_known","know"),("has_known","know"),("measured","measure"),("measure","measure"),("measures","measure"),("will_measure","measure"),("have_measured","measure"),("has_measured","measure"),("missed","miss"),("miss","miss"),("misses","miss"),("will_miss","miss"),("have_missed","miss"),("has_missed","miss"),("nudged","nudge"),("nudge","nudge"),("nudges","nudge"),("will_nudge","nudge"),("have_nudged","nudge"),("has_nudged","nudge"),("obtained","obtain"),("obtain","obtain"),("obtains","obtain"),("will_obtain","obtain"),("have_obtained","obtain"),("has_obtained","obtain"),("owned","own"),("own","own"),("owns","own"),("will_own","own"),("have_owned","own"),("has_owned","own"),("ate","eat"),("eat","eat"),("eats","eat"),("will_eat","eat"),("have_eaten","eat"),("has_eaten","eat"),("pitied","pity"),("pity","pity"),("pities","pity"),("will_pity","pity"),("have_pitied","pity"),("has_pitied","pity"),("preserved","preserve"),("preserve","preserve"),("preserves","preserve"),("will_preserve","preserve"),("have_preserved","preserve"),("has_preserved","preserve"),("pushed","push"),("push","push"),("pushes","push"),("will_push","push"),("have_pushed","push"),("has_pushed","push"),("raced","race"),("race","race"),("races","race"),("will_race","race"),("have_raced","race"),("has_raced","race"),("received","received"),("received","received"),("receives","received"),("will_received","received"),("have_received","received"),("has_received","received"),("rejected","reject"),("reject","reject"),("rejects","reject"),("will_reject","reject"),("have_rejected","reject"),("has_rejected","reject"),("remembered","remember"),("remember","remember"),("remembers","remember"),("will_remember","remember"),("have_remembered","remember"),("has_remembered","remember"),("killed","kill"),("kill","kill"),("kills","kill"),("will_kill","kill"),("have_killed","kill"),("has_killed","kill"),("rode","ride"),("ride","ride"),("rides","ride"),("will_ride","ride"),("have_ridden","ride"),("has_ridden","ride"),("sought","seek"),("seek","seek"),("seeks","seek"),("will_seek","seek"),("have_sought","seek"),("has_sought","seek"),("seized","seize"),("seize","seize"),("seizes","seize"),("will_seize","seize"),("have_seized","seize"),("has_seized","seize"),("shaved","shave"),("shave","shave"),("shaves","shave"),("will_shave","shave"),("have_shaved","shave"),("has_shaved","shave"),("stung","sting"),("sting","sting"),("stings","sting"),("will_sting","sting"),("have_stung","sting"),("has_stung","sting"),("tested","test"),("test","test"),("tests","test"),("will_test","test"),("have_tested","test"),("has_tested","test")]

baseToPres = [("shout","shouts"),("age","ages"),("arrive","arrives"),("ache","aches"),("advance","advances"),("ail","ails"),("apologize","apologizes"),("boast","boasts"),("breathe","breathes"),("breed","breeds"),("chant","chants"),("cheat","cheats"),("come","comes"),("cry","cries"),("dance","dances"),("decay","decays"),("delay","delays"),("deliberate","deliberates"),("depreciate","depreciates"),("enter","enters"),("escape","escapes"),("exhale","exhales"),("explode","explodes"),("fail","fails"),("fly","flies"),("function","functions"),("gamble","gambles"),("happen","happens"),("hesitate","hesitates"),("hurry","hurries"),("lie","lies"),("meditate","meditates"),("occur","occurs"),("pose","poses"),("pray","prays"),("react","reacts"),("rebel","rebels"),("relax","relaxes"),("rest","rests"),("run","runs"),("scream","screams"),("shine","shines"),("shrink","shrinks"),("sink","sinks"),("sit","sits"),("sleep","sleeps"),("slip","slips"),("sneak","sneaks"),("snore","snores"),("speak","speaks"),("spit","spits"),("squeak","squeaks"),("stink","stinks"),("sweat","sweats"),("freeze","freezes"),("abandon","abandons"),("abhor","abhors"),("avoid","avoids"),("absorb","absorbs"),("abuse","abuses"),("accomodate","accomodates"),("accompany","accompanies"),("achieve","achieves"),("acquire","acquires"),("adopt","adopts"),("aid","aids"),("alarm","alarms"),("amaze","amazes"),("amuse","amuses"),("annoy","annoys"),("approach","approaches"),("astound","astounds"),("await","awaits"),("ban","bans"),("become","becomes"),("believe","believes"),("break","breaks"),("capture","captures"),("chase","chases"),("chew","chews"),("chop","chops"),("devour","devours"),("discipline","disciplines"),("disobey","disobeys"),("find","finds"),("flatter","flatters"),("follow","follows"),("handle","handles"),("hear","hears"),("help","helps"),("imitate","imitates"),("import","imports"),("include","includes"),("know","knows"),("measure","measures"),("miss","misses"),("nudge","nudges"),("obtain","obtains"),("own","owns"),("eat","eats"),("pity","pities"),("preserve","preserves"),("push","pushes"),("race","races"),("received","receives"),("reject","rejects"),("remember","remembers"),("kill","kills"),("ride","rides"),("seek","seeks"),("seize","seizes"),("shave","shaves"),("sting","stings"),("test","tests")]

