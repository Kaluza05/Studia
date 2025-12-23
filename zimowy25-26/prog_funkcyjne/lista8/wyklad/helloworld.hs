hello :: IO ()
hello = putStrLn "Hello World!!!"

main :: IO ()
main = do
  hello
  hello
  x <- readLn
  putStrLn $ show $ x + 1

-- do { x <- a ; b } ---> a >>= \x -> b