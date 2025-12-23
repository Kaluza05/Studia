{-# LANGUAGE LambdaCase #-}

import Data.Char (toLower)
import System.IO (isEOF)

echoLower :: IO ()
echoLower = do
    eof <- isEOF
    if eof
        then return ()
        else do
            c <- getChar
            putChar $ toLower c
            echoLower


data StreamTrans i o a = 
    Return a
    | ReadS (Maybe i -> StreamTrans i o a)
    | WriteS o (StreamTrans i o a)


toLowerST :: StreamTrans Char Char ()
toLowerST = ReadS $ \case
        Nothing -> Return ()
        Just i  -> WriteS (toLower i) toLowerST
        


runIOStreamTrans :: StreamTrans Char Char a -> IO a
runIOStreamTrans (Return a) = return a

runIOStreamTrans (ReadS f) = do
    eof <- isEOF
    if eof
        then  runIOStreamTrans (f Nothing)
    else do
        c <- getChar
        runIOStreamTrans $ f $ Just c

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

--put output to the input
runCycle :: StreamTrans a a b -> b
runCycle s = last_el s [] where
    last_el (Return b) _ = b
    last_el (ReadS f) [] = last_el (f Nothing) []
    last_el (ReadS f) (j:l)  = last_el (f j) l
    last_el (WriteS o cont) l = last_el cont (l ++ [Just o])

-- runCycle t = out where
    -- (xs,out) = listTrans t xs
    

(|>|) :: StreamTrans i m a -> StreamTrans m o b -> StreamTrans i o b
(|>|) _ (Return b) = Return b
(|>|) (WriteS o cont) (ReadS f) = cont |>| f (Just o) --przekierowany output na input
(|>|) s (WriteS o cont) = WriteS o (s |>| cont)
(|>|) (ReadS f) s = ReadS (\i -> f i |>| s)
(|>|) (Return a) (ReadS f) = Return a |>| f Nothing

catchOutput :: StreamTrans i o a -> StreamTrans i b (a, [o])
catchOutput s = wrap_out s [] where
    wrap_out (Return x) l = Return (x,reverse l)
    wrap_out (ReadS f)  l = ReadS (\j -> wrap_out (f j) l)
    wrap_out (WriteS o cont) l = wrap_out cont (o : l)