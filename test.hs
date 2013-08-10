module Test where
  import Ast
  import Eval
  import Data.Word
  import System.Random
  import Data.Bits

--  instance Random Word64 where
--    randomR (lo, hi) g = (fromInteger n, g')
--      where (n, g') = (randomR (toInteger lo, toInteger hi) g)
--    random g = randomR (minBound, maxBound) g
  
  simpleTests :: [Word64]
  simpleTests = [0, -1] ++ [shiftL 1 n | n <- [0 .. 63]] ++ [rotateL (-2) n | n <- [0 .. 63]]

  test :: [Prog] -> IO ([Word64], [([Word64], [Prog])])
  test progs = testAux 1024 (testKnown simpleTests ([], [([], progs)]))  
               -- ([], [([], progs)])              
               
  testKnown [] res = res
  testKnown (t : tests) (gtests, buckets) =
    let (buckets', split) = foldr (splitBucket t) ([], False) buckets
    in if split then testKnown tests (t : gtests, buckets')
       else testKnown tests (gtests, buckets)

  testN n progs = detTestAux n [100001*x | x<-[1..1000]] (testKnown simpleTests ([], [([], progs)]))  
               -- ([], [([], progs)])              
       
--  detTestAux :: Integer -> [Word64] -> ([Word64], [([Word64], [Prog])]) -> ([Word64], [([Word64], [Prog])])
  detTestAux runs rand @ (x:xs) (tests, buckets) =
    if runs == 0 || length tests == 256 || all (\(rs, ps) -> length ps == 1) buckets 
    then (tests, buckets)
    else 
      let (buckets', split) = foldr (splitBucket x) ([], False) buckets in
      if split then detTestAux  (runs - 1) xs (x : tests, buckets')
        else detTestAux (runs - 1) xs (tests, buckets)
     
       
  testAux :: Integer -> ([Word64], [([Word64], [Prog])]) -> IO ([Word64], [([Word64], [Prog])])
  testAux runs (tests, buckets) =
    if runs == 0 || length tests == 256 || all (\(rs, ps) -> length ps == 1) buckets 
    then return (tests, buckets)
    else do
      n <- randomIO
      let (buckets', split) = foldr (splitBucket n) ([], False) buckets
      if split then testAux (runs - 1) (n : tests, buckets')
        else testAux (runs - 1) (tests, buckets)

  splitBucket n (ress, progs) (bs, flag) =
    let insert [] n p = [(n:ress, [p])]
        insert ((m:ress, ps) : bs) n p =
          if n == m then ((m : ress, p : ps) : bs)
          else (m : ress, ps) : insert bs n p
        aux bucks [] = bucks
        aux bucks (p : progs) =
          aux (insert bucks (eval p n) p) progs
        nbucks = aux [] progs
    in (nbucks ++ bs, if length nbucks == 1 then flag else True)