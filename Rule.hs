module Rule where

import Data.Set( Set )
import qualified Data.Set as S
import Data.Map( Map )
import qualified Data.Map as M
import Data.List( sort, insert, group, groupBy )
import Debug.Trace

--------------------------------------------------------------------------------

type Table item answer = [(Int,Entry item answer)]
type Entry item answer = (Set item, answer)
type Rule  item answer = (Set item, answer)

--------------------------------------------------------------------------------

findRulesIO :: 
  (Ord item, Ord answer) =>
    Table item answer -> (answer -> Bool) -> (item -> Int)
                      -> (Rule item answer -> String) -> IO [Rule item answer]
findRulesIO table valid cost shw = go table rules
 where
  rules = findRules table valid cost
 
  go table [] =
    do putStrLn ("+++ done (" ++ show (length table) ++ " left)")
       return rules

  go table (r:rs) =
    do putStrLn ( shw r
               ++ " -"
               ++ show (length table - length table')
               ++ " /" ++ show (length table')
                )
       go table' rs
   where
    table' = apply r table

--------------------------------------------------------------------------------

findRules ::
  (Ord item, Ord answer) =>
    Table item answer -> (answer -> Bool) -> (item -> Int)
                                                  -> [Rule item answer]
findRules table valid cost = find table []
 where
  find table cands =
    case newRules table cands of
      []          -> []
      rule:cands' -> correct rule $ rule : find (apply rule table) cands'

  newRules [] _ =
    []

  newRules table cands =
    case take 25 -- arbitrary number
       . reverse
       . sort
       . filter ((>0) . fst . fst)
       $ [ (gainFor rule table cost, rule) | rule <- cands ] of
      []                 -> bestRules table valid cost (Nothing,gain0)
      (gain,rule):cands' -> tempTrace (show (fst gain) ++ "!") $
                            bestRules table valid cost (Just rule,gain)
                              ++ map snd cands'

  correct (pre,ans) x
    | null [ e | (_,e@(pre',ans')) <- table, pre `S.isSubsetOf` pre', ans /= ans' ] =
        x
    | otherwise =
        error "NO RULE"

--------------------------------------------------------------------------------

type Gain = (Int,[Int])

gain0 :: Gain
gain0 = (0,[])

apply :: 
  (Ord item, Ord answer) =>
    Rule item answer -> Table item answer -> Table item answer
apply (is,_) table = [ e | e@(_,(is',_)) <- table, not (is `S.isSubsetOf` is') ]

gainFor :: 
  (Ord item, Ord answer) =>
    Rule item answer -> Table item answer -> (item -> Int) -> Gain
gainFor (is,_) table cost =
  ( sum [ c | (c,(is',_)) <- table, is `S.isSubsetOf` is' ]
  , sort [ -cost i | i <- S.toList is ]
  )

--------------------------------------------------------------------------------

bestRules ::
  (Ord item, Ord answer) =>
    Table item answer -> (answer -> Bool) -> (item -> Int)
                         -> (Maybe (Rule item answer), Gain) ->
      [Rule item answer]
bestRules table valid cost (mrule,gain) = bestRulesFor as (mrule,gain)
 where
  as = filter valid
     . map snd
     . reverse
     . sort
     . map (\(a,k) -> (k,a))
     . M.toList
     . M.fromListWith (+)
     $ [ (a,c) | (c,(_,a)) <- table ]

  bestRulesFor [] (mrule,gain) =
    [ rule | Just rule <- [mrule] ]

  bestRulesFor (a:as) (mrule,gain) =
    case bestPatterns []
                      [ (c,is) | (c,(is,b)) <- table, b /= a ]
                      [ (c,is) | (c,(is,b)) <- table, b == a ] cost gain of
      []              -> bestRulesFor as (mrule,gain)
      (pat,gain):pats -> bestRulesFor as (Just (pat,a),gain) ++~
                         ([ r | Just r <- [mrule] ] ++ [ (is,a) | (is,_) <- pats ])

--------------------------------------------------------------------------------

bestPatterns ::
  (Ord item) =>
    [item] -> [(Int,Set item)] -> [(Int,Set item)] -> (item -> Int) -> Gain ->
      [(Set item, Gain)]
bestPatterns is no yes cost gain
  | null items || poss <= gain =
      []
  
  | null no =
      tempTrace (show (fst poss)) $
        [(S.fromList is, poss)]

  | otherwise =
      case bestPatterns (item:is) no1 yes1 cost gain of
        []                 -> bestPatterns is no2 yes2 cost gain
        pats@((_,gain'):_) -> bestPatterns is no2 yes2 cost gain' ++~ pats
 where
  poss = ( sum (map fst yes)
           -- if null no
           --   then length yes
           --   else maximum [ n | ((_,n,_),_) <- items ]
         , sort [ -cost i | i <- is ]
         )
    
  items =
    [ ( ( sum [ c | (c,is) <- no,  not (i `S.member` is) ]
        , sum [ c | (c,is) <- yes, i `S.member` is ]
        , -(cost i)
        )
      , i
      )
    | i <- S.toList (foldr S.union S.empty (map snd yes))
    ]

  item = snd (maximum items)

  no1  = [ (c,S.delete item is) | (c,is) <- no,  item `S.member` is ]
  yes1 = [ (c,S.delete item is) | (c,is) <- yes, item `S.member` is ]

  no2  = map (\(is,c) -> (c,is))
       $ M.toList
       $ M.fromListWith (+) [ (S.delete item is,c) | (c,is) <- no ]
  yes2 = [ (c,S.delete item is) | (c,is) <- yes ]

--------------------------------------------------------------------------------

tempTrace :: String -> a -> a
tempTrace s = trace (s' ++ replicate (length s') '\b' ++ "\ESC[A")
 where
  s' = "(" ++ s ++ ") "

{-
nub :: Ord a => [a] -> [a]
nub = go S.empty
 where
  go seen []                         = []
  go seen (x:xs) | x `S.member` seen = go seen xs
                 | otherwise         = x : go (S.insert x seen) xs
-}

(++~) :: [a] -> [a] -> [a]
[]     ++~ ys = ys
(x:xs) ++~ ys = x:(ys ++ xs)

--------------------------------------------------------------------------------

