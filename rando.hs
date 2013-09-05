import Prelude
import Control.Applicative
import Data.Random (runRVar, StdRandom(..), randomElement, shuffle)

data Symbol = On | Off
instance Show Symbol where
	show On = "H"
	show Off = "T"

randomSymbol :: IO Symbol
randomSymbol = runRVar (randomElement [On, Off]) StdRandom

boxed :: [String] -> String
boxed xs = header ++ unlines (map bracket xs) ++ footer where
	len = maximum (map length xs)
	header = '┌' : (take len $ repeat '─') ++ "┐\n"
	bracket line = '│' : (take len $ line ++ repeat ' ') ++ "│"
	footer = '└' : (take len $ repeat '─') ++ "┘"

puzzle :: Int -> [Symbol] -> String
puzzle _ [] = ""
puzzle n ys = boxed $ map (concatMap show) (groupsOf n ys)

groupsOf :: Int -> [a] -> [[a]]
groupsOf _ [] = []
groupsOf n xs = (take n xs) : groupsOf n (drop n xs)

chunk :: Int -> IO [Symbol]
chunk n = runRVar symlist StdRandom where
	symlist = shuffle (take n (repeat On) ++ take n (repeat Off))

fake :: Int -> Int -> IO [Symbol]
fake width count = concat <$> sequence symbols where
	symbols = take (quot count (width * 2)) (repeat $ chunk width)

rando :: Int -> IO [Symbol]
rando n = sequence $ take n $ repeat randomSymbol

main :: IO ()
main = do
	let width = 16
	let size = width * 4

	putStrLn "Hard mode? [y/n]"
	wantsHard <- getLine
	let playHard = wantsHard == "y"
	let (name0, board0) = if playHard
		then ("cell-4", fake 2)
		else ("cell-2", fake 1)
	let (name1, board1) = if playHard
		then ("cell-8", fake 4)
		else ("cell-4", fake 2)

	boards <- sequence [board0 size, board1 size, rando size]
	key <- runRVar (shuffle [0, 1, 2]) StdRandom
	putStrLn "1."
	putStrLn $ puzzle width $ boards !! (key !! 0)
	putStrLn "2."
	putStrLn $ puzzle width $ boards !! (key !! 1)
	putStrLn "3."
	putStrLn $ puzzle width $ boards !! (key !! 2)

	guess <- getLine
	if not (guess `elem` ["1", "2", "3"])
	then putStrLn "fail"
	else if key !! ((read guess) - 1) == 2
		then putStrLn "win"
		else putStrLn "lose"

	let names = [name0, name1, "random"]
	let reveal (n, b) = putStrLn $ (show n) ++ ": " ++ (names !! b)
	mapM_ reveal (zip [1..] key)
