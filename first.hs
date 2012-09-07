module Main where
import System.Environment

main :: IO ()
main = do
--    putStr $ "input your first name: "
    firstName <- getLine
--    putStr $ "input your last name: "    
    lastName  <- getLine
    putStrLn $ "Hello, " ++ firstName ++ lastName
    
--    args <- getArgs
--    putStrLn . show . sum $ map (read) args
--    putStrLn(show $ (read (args !! 0) :: Int) + (read (args !! 1) :: Int))
--    putStrLn("Hello, " ++ args !! 0 ++ " " ++ args !! 1)