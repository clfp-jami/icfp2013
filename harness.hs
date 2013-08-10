module Harness where

  import Ast
  import Gen
  import Test
  import Numeric
  import Data.Word
  
  churn progs =
    do
      putStrLn $ "Number of programs generated: " ++ show (length progs)
      (tests, buckets) <- test progs
      putStrLn $ "Number of tests generated: " ++ show (length tests)
      putStrLn $ "Maximal number of programs in a bucket: " ++
        show (maximum (map (\(rs, ps) -> length ps) buckets))
      return (tests, buckets)

  -- generate all the "simple" programs (w/o folds), get the tests and the result-coded buckets of programs
  -- returns a pair (tests, buckets); use the buckets later w/ findProgs function
  runSimple depth uops bops canIf =
    let progs = genProgs depth uops bops canIf
    in churn progs

  runSimpler depth ops =
    let (uops, bops, canIf, fold, tfold) = parse ops 
	in let f = if tfold then runTFold else if fold then runFold else runSimple
    in f depth uops bops canIf
	
  -- generate all the programs with the 'tfold' operation, get the tests and the result-coded buckets of programs
  -- returns a pair (tests, buckets); use the buckets later w/ findProgs function
  runTFold depth uops bops canIf =
    let progs = genProgsTFold depth uops bops canIf
    in churn progs

  runFold depth uops bops canIf =
    do
      putStrLn $ "Not implemented yet" 
      return ([], [])
	
	
  -- prints a list of words (e.g., tests) in hexadecimal. Also prints a trailing comma, cause I'm lazy
  printTests :: [Word64] -> IO ()
  printTests = mapM_ (putStrLn . flip showHex ", ")

  -- find the programs that match the list of results
  findProgs results buckets =
    lookup results buckets
	
  parse string = parseArr string [] [] False False False
	
  parseArr [] uops bops canIf fold tfold = (uops, bops, canIf, fold, tfold)
  parseArr (x:xs) uops bops canIf  fold tfold = parseArr xs uops' bops' canIf' fold' tfold'
    where (uop, bop, arity) = optopr x	
          canIf' = (canIf || arity == 3)
          fold'  = fold || arity == 4
          tfold' = tfold || arity == 5
          uops'  = if arity == 1 then (uop:uops) else uops
          bops'  = if arity == 2 then (bop:bops) else bops
		  
		  