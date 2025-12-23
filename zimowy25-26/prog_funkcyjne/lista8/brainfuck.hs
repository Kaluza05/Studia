{-# LANGUAGE LambdaCase #-}


import Control.Monad
import Control.Monad.Cont (cont)
import System.IO

data StreamTrans i o a = 
    Return a
    | ReadS (Maybe i -> StreamTrans i o a)
    | WriteS o (StreamTrans i o a)



instance Functor (StreamTrans i o) where
    fmap f (Return a)       = Return (f a)
    fmap f (ReadS k)        = ReadS (fmap f . k )
    fmap f (WriteS o rest)  = WriteS o (fmap f rest)

instance Applicative (StreamTrans i o) where
  pure = Return
  (<*>) = ap

instance Monad (StreamTrans i o) where
  return = pure
  (>>=) (Return a) f = f a
  (>>=) (ReadS s) f = ReadS(\j -> s j >>= f)
  (>>=) (WriteS o c) f = WriteS o (c >>= f)


data BF = 
    MoveR -- >
    | MoveL -- <
    | Inc -- +
    | Dec -- -
    | Output -- .
    | Input -- ,
    | While [BF] -- [ ]
    deriving Show



-- lista do której bezie się zbierać liste bf dopoki nie bedzie nawiasu zamykajacego
brainfuckParser :: StreamTrans Char BF ()
brainfuckParser = remember_while [] where
    remember_while :: [[BF]] -> StreamTrans Char BF ()
    remember_while stack = ReadS $ \case
        Nothing -> Return ()
        Just j  ->  
                case j of
                '>' -> run_stack MoveR   stack
                '<' -> run_stack MoveL   stack
                '+' -> run_stack Inc     stack
                '-' -> run_stack Dec     stack
                '.' -> run_stack Output  stack
                ',' -> run_stack Input   stack
                '[' -> remember_while ([] : stack)
                ']' -> 
                    case stack of 
                        [] -> error "wrong parsing"             
                        l : stack ->  run_stack (While $ reverse l) stack
                        -- doczepiamy blok do poprzenidej listy bo dalej jestesmy w jakims while-u
                _   -> remember_while stack
            
    -- if the stack is empty we dont want to 

    run_stack :: BF -> [[BF]] -> StreamTrans Char BF ()
    run_stack mv [] = WriteS mv (remember_while [])
    run_stack mv (l : ls) = remember_while $ (mv : l) : ls



type Tape = ([Integer], Integer, [Integer])
evalBF :: Tape -> BF -> StreamTrans Char Char Tape
evalBF (l,x, y : r) MoveR = Return (x : l, y, r)
evalBF (y : l, x,r) MoveL = Return (l, y,  x : r)
evalBF (l,x,r) Inc = Return (l,x + 1, r)
evalBF (l,x,r) Dec = Return (l,x - 1,r)
evalBF (l,x,r) Output     = WriteS (coerceEnum x) (Return (l,x,r))
evalBF (l,x,r) Input = ReadS $ \case 
        Nothing -> Return (l,x,r)
        Just j  -> Return (l,coerceEnum j,r)
evalBF (l,0,r) (While _) = return (l,0,r)  --0 on the tape so dont do the loop
evalBF t (While l) = do
    t <- evalBFBlock t l
    evalBF t (While l)
 -- non 0 so run loop and see if i need to run it again


evalBFBlock :: Tape -> [BF] -> StreamTrans Char Char Tape
evalBFBlock = foldM evalBF 


runIOStreamTrans :: StreamTrans Char Char a -> IO a
runIOStreamTrans (Return a) = return a

runIOStreamTrans (ReadS f) = do
    eof <- isEOF
    if eof
        then  runIOStreamTrans (f Nothing)
    else do
        c <- getChar
        runIOStreamTrans (f (Just c))

runIOStreamTrans (WriteS o cont) = do
    putChar o
    runIOStreamTrans cont


listTrans :: StreamTrans i o a -> [i] -> ([o], a)
listTrans (Return x) _ = ([],x)

listTrans (ReadS f) [] = --waits for something but theres nothing 
    listTrans (f Nothing) []

listTrans (ReadS f) (x : xs) = --theres x that we can read and repreat the process on xs
    listTrans (f $ Just x) xs

listTrans (WriteS o cont) xs = --append the output to the output list
    let (rest,ret) = listTrans cont xs in (o : rest, ret)

parseBF :: [Char] -> [BF]
parseBF  = fst . listTrans brainfuckParser 

runBF :: [Char] -> IO ()
runBF src = do
  let bfCode = parseBF src
      st = evalBFBlock ([0,0..], 0,[0,0..]) bfCode
  runIOStreamTrans (st >> Return ())

coerceEnum :: (Enum a, Enum b) => a -> b
coerceEnum = toEnum . fromEnum

helloWorld = ">++++++++[<+++++++++>-]<.>++++[<+++++++>-]<+.+++++++..+++.>>++++++[<+++++++>-]<+\
\+.------------.>++++++[<+++++++++>-]<+.<.+++.------.--------.>>>++++[<++++++++>-]<+."

main :: IO ()
main = runBF helloWorld