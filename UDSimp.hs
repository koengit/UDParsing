module Main where

import Prelude hiding ( Word )
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Char
import Data.List
import Data.Ord
import Test.QuickCheck hiding ( tabulate )
import Rule hiding (apply)
import Control.Exception

--------------------------------------------------------------------------------

-- what corpus to use
--lang = "en"
--lang = "cop"
--lang = "af_afribooms"
--lang = "fo_oft"
--lang = "swl_sslc"
--lang = "cy_ccg"
lang = "ja_modern"
--lang = "pcm_nsc"
--lang = "hy_armtdp"
--lang = "kmr_mg2"
--lang = "kk_ktb2"

-- max #sentences for the training corpus
maxTraining = Nothing

-- min #sentences for the testing corpus
minTesting = 100

-- should only the main tag (NOUN, VERB, etc.) be used or the other tags too?
onlyMainTag = False -- should be False

-- can the rules make mistakes in the training corpus or not?
preciseRules = True -- should be True

--------------------------------------------------------------------------------

main :: IO ()
main =
  do msL <- try $ readFile ("corpus/" ++ lang ++ "-ud-train.conllu")
     msT <- try $ readFile ("corpus/" ++ lang ++ "-ud-test.conllu")
     
     let cL = case maxTraining of
                Nothing -> cL'
                Just k  -> take k cL'
     
         (cL',cT) =
           case (msL, msT) of
             (Left e,  Right _) -> split cT0 where types = e :: IOError
             (Right _, Left e)  -> split cL0 where types = e :: IOError
             (Right _, Right _) -> (cL0, cT0)
     
         cL0 = parseCorpus (case msL of Right sL -> sL)
         cT0 = parseCorpus (case msT of Right sT -> sT)
     
         split c = (take l c, drop l c)
          where
           n = length c
           l = if n > minTesting then n-minTesting else n `div` 2
     
     putStrLn ( "language: " ++ lang ++ ": "
             ++ show (length cL) ++ " training, "
             ++ show (length cT) ++ " testing"
              )
     
     putStrLn ( "    score-L    | "
             ++ "                               rule                                | "
             ++ "   score-T"
              ) 
     let go cL cT =
           let tab = tabulate cL in
           if null tab then
             do putStrLn "-- DONE --"
           else
             do writeFile "corpus-L" $ unlines $
                  [ showSent s | s <- cL ]
                writeFile "corpus-T" $ unlines $
                  [ showSent s | s <- cT ]
                putStr ( rjust 14 (score cL)
                      ++ " | "
                       )
                case findRules [ r
                               | r@(_,(_,ans)) <- tab
                               , preciseRules || (ans `elem` [LArrow,RArrow])
                               ] valid cost of
                  [] ->
                    do putStrLn "!!NO RULE!!"
     
                  r:_ ->
                    do let cL' = apply r cL
                           cT' = apply r cT
                       putStrLn ( spread (showRule r)
                               ++ " | "
                               ++ score cT'
                                )
                       {- if reverse (take 6 (reverse (score cL'))) /= "100.0%"
                         then do writeFile "corpus-L2" $ unlines $
                                   [ showSent s | s <- cL' ]
                                 putStrLn " :-o :-o :-o (press key)"
                                 getLine
                                 return ()
                         else return () -}
                       go cL' cT'

     go (corpus cL) (corpus cT)
 where
  spread s =
    rjust 30 (unwords ws1) ++ " " ++ op ++ " " ++ ljust 30 (unwords ws2)
   where
    ws     = words s
    isOp s = length [ '-' | '-' <- s ] >= 2
    ws1    = takeWhile (not . isOp) ws
    op     = head (filter isOp ws)
    ws2    = drop 1 (dropWhile (not . isOp) ws)

showSent ((Node w1 _,b):ws@((Node w2 _,_):_)) =
    head (tags w1)
    ++ " "
    ++ (if fst (arrow w1) == wid w2 then "> " else "")
    ++ (if fst (arrow w2) == wid w1 then "< " else "")
    ++ (if b then "" else "¤ ")
    ++ showSent ws

showSent [(Node w1 _,_)] =
    head (tags w1)

--------------------------------------------------------------------------------

data Trait
  = Tag String
  | Children Int
  | AtLeastOneChild
  | ChildTag String
  | Boundary
  | IsWord
 deriving ( Eq, Ord, Show )

cost :: (Int,Trait) -> Int
cost (i,t) = dist i + c t
 where
  dist i | i <= 0    = 5 * (-i)
         | otherwise = 5 * (i-1)
 
  c (Tag s) | not (all isUpper s) = 2
  c (Children _)    = 3
  c (ChildTag _)    = 3
  c AtLeastOneChild = 4
  c _               = 1

data Action
  = LArrow
  | RArrow
  | NoArrow
  | NotYet
 deriving ( Eq, Ord, Show )

valid :: Action -> Bool
valid NotYet = False
--valid NoArrow = False
valid _      = True

data Tree
  = Node Word [Tree]
 deriving ( Eq, Ord, Show )

type Corpus
  = [[(Tree,Bool)]]

score :: Corpus -> String
score c = rjust 5 (showp mn) ++ " -" ++ rjust 6 (showp mx)
 where
  mn = (1000 * good) `div` tot
  mx = (1000 * (tot-bad)) `div` tot
  
  (tot,good,bad) = fcorpus c
  
  fcorpus c = (sum tots, sum goods, sum bads)
   where
    (tots,goods,bads) = unzip3 [ fsent s | s <- c ]
  
  fsent s =
    ( sum tots  + length s
    , sum goods + if length s == 1 && all ((==0) . fst . arrow) [ w | (Node w _,_) <- s ]
                    then 1
                    else 0
    , sum bads  + length [ w | (Node w _,_) <- s, fst (arrow w) `notElem` ids ]
                + length [ w | ((Node w _,False),(Node v _,_)) <- s `zip` tail s
                             , fst (arrow w) == wid v || fst (arrow v) == wid w
                             ]
    )
   where
    ids = 0:[ wid w | (Node w _,_) <- s ]
    (tots,goods,bads) = unzip3 [ ftree t | (t,_) <- s ]
  
  ftree (Node w ts) =
    ( sum tots  + length ts
    , sum goods + length [ c | Node c _ <- ts, fst (arrow c) == wid w ]
    , sum bads  + length [ c | Node c _ <- ts, fst (arrow c) /= wid w ]
    )
   where
    (tots,goods,bads) = unzip3 [ ftree t | t <- ts ]

  showp p = show (fromIntegral p / 10) ++ "%"

--------------------------------------------------------------------------------

traits :: [Maybe Tree] -> S.Set (Int,Trait)
traits ts = S.fromList [ (i,p) | (i,mt) <- is `zip` ts, p <- trs mt ]
 where
  n  = length ts
  is = take n [(1-(n`div`2))..]

  trs Nothing =
    [ Boundary ]
  
  trs (Just (Node w ts)) =
    [ Tag t | t <- (if onlyMainTag then take 1 else id) $ tags w ] ++
    [ IsWord
    , Children (length ts)
    ] ++
    [ AtLeastOneChild | not (null ts) ] ++
    [ ChildTag t | Node c _ <- ts, t <- (if onlyMainTag then take 1 else id) $ tags c ]

showRule :: Rule (Int,Trait) Action -> String
showRule (pres,act) =
  concat [ word i
        ++ (if i == 0 then " " ++ action ++ " " else if i < mx then " • " else "")
         | i <- [mn..mx]
         ]
 where
  pre = S.toList pres
  mn  = minimum (0:[ i | (i,_) <- pre ])
  mx  = maximum (1:[ i | (i,_) <- pre ])

  word i = par ++ (if null chd then "" else if chd == "0" then "()" else "(" ++ chd ++ ")")
   where
    trs = [ tr | (j,tr) <- pre, j == i ]
    
    par = head $
          [ tag  | Tag tag <- trs ] ++
          [ "##" | Boundary <- trs ] ++
          [ "<word>" | IsWord <- trs ] ++
          [ "_" ]
    
    chd = concat $ intersperse "," $
          [ show n | Children n <- trs ] ++
          [ "_"    | AtLeastOneChild <- trs ] ++
          [ t      | ChildTag t <- trs ]

  action =
    case act of
      LArrow  -> "<---"
      RArrow  -> "--->"
      NoArrow -> "-xx-"
      NotYet  -> "-??-"

apply :: Rule (Int,Trait) Action -> Corpus -> Corpus
apply (pres,arr) corpus = [ app s | s <- corpus ]
 where
  app s = trans (if arr == LArrow then reverse ks else ks) ([0..] `zip` s)
   where
    ks = [ k
         | (k,(_,True)) <- [0..] `zip` init s
         , let trs = traits (take 4 (drop k ([Nothing]++[Just t|(t,_)<-s]++[Nothing])))
         , pres `S.isSubsetOf` trs
         ]
   
    trans []     s = map snd s
    trans (k:ks) s = trans ks (rew k s)
   
    rew k s =
      case arr of
        NoArrow -> tak ++ [ (i1,(t1,False)), p2 ] ++ drp2
        LArrow  -> tak ++ [ (i1,(Node w1 (ts1++[t2]),True)) ] ++ drp2
        RArrow  -> true tak ++ [ (i2,(Node w2 (t1:ts2),b2)) ] ++ drp2
     where
      (tak,
        p1@(i1,(t1@(Node w1 ts1),b1)),
        p2@(i2,(t2@(Node w2 ts2),b2)),
        drp2) = find k [] s
      
      find k kat (p@(i,_):q:ps)
        | k == i    = (reverse kat,p,q,ps)
        | otherwise = find k (p:kat) (q:ps)
      
      true [(i,(t,_))] = [(i,(t,True))]
      true (p:ps)      = p : true ps
      true []          = []
            
--------------------------------------------------------------------------------

corpus :: [Sentence] -> Corpus
corpus sents = [ [ (Node w [],True) | w <- s ] | s <- sents ]

tabulate :: Corpus -> Table (Int,Trait) Action
tabulate sents =
  count [ (traits ts, action)
        | s <- sents
        , (k,(Node w1 _,True)) <- [0..] `zip` init s
        , let (Node w2 _, _) = s !! (k+1)
        , let ts = take 4 (drop k ([Nothing]++[Just t|(t,_)<-s]++[Nothing]))
        , let action
                | fst (arrow w1) == wid w2 =
                    if or [ fst (arrow w) == wid w1 | (Node w _,_) <- s ]
                      then NotYet else RArrow
                | fst (arrow w2) == wid w1 =
                    if or [ fst (arrow w) == wid w2 | (Node w _,_) <- s ]
                      then NotYet else LArrow
                | otherwise =
                    NoArrow
        ]
 where
  count xs = map swap $ M.toList $ M.fromListWith (+) [ (x,1) | x <- xs ]
   where
    swap (x,n) = (n,x)

--------------------------------------------------------------------------------

type Sentence = [Word]
type Tag      = String
type Lab      = String

data Word = Word
  { wid   :: Int
  , word  :: String
  , lemma :: String
  , tags  :: [Tag]
  , arrow :: (Int,Lab)
  }
 deriving ( Eq, Ord, Show )

parseCorpus :: String -> [Sentence]
parseCorpus s =
  sentences
    [ Word (read (cs!!0))
           (cs!!1)
           (cs!!2)
           ((cs!!3):(if ts == ["_"] then [] else map secondary ts))
           (read (cs!!6), trunc (cs!!7))
    | l <- lines s
    , let cs = cols '\t' l
    , length cs >= 7
    , all isDigit (cs!!0)
    , let ts = cols '|' (cs!!5) 
    ]
 where
  sentences []     = []
  sentences (w:ws) = (w:takeWhile ((>1).wid) ws) : sentences (dropWhile ((>1).wid) ws)

  cols x = takeWhile (not . null)
         . map (takeWhile (/=x))
         . iterate (drop 1 . dropWhile (/=x))

  trunc = takeWhile (/=':')

  secondary t | '=' `elem` t = t
              | otherwise    = '=':t

--------------------------------------------------------------------------------

ljust, rjust :: Int -> String -> String
ljust n s = s ++ replicate (n - length s) ' '
rjust n s = replicate (n - length s) ' ' ++ s

showp :: (Int,Int) -> String
showp (_,0) = "-%"
showp (a,t) = show (fromIntegral ((1000 * a) `div` t) / 10) ++ "%"

