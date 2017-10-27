import Data.List

main = do
	
	putStrLn "The RSA breaker ..."
	putLine
	
	putStr "Input RSA key: "
	key <- getLine
	let rsaKey = read key :: Integer
	
	putStr "Input RSA base: "
	base <- getLine
	let rsaBase = read base :: Integer
	
	putStrLn ("Key: " ++ key ++ "\nBase:" ++ base)
	putLine
	
	putStrLn "Searching for possible keys:"
	print (breakKey 2 (rsaKey, rsaBase))
	
	
	where
		putLine = putStrLn (take 30 (repeat '='))

breakKey :: Integer -> (Integer, Integer) ->  Maybe Integer
breakKey secret (key, base) = find (tryKey base secret encryption) [1..base]
	where
		encryption = encrypt secret (key, base)
		candidates = [1..base]

tryKey :: Integer -> Integer -> Integer -> Integer -> Bool
tryKey base secret encryption testKey = secret == encryption^testKey `mod` base

encrypt :: Integer -> (Integer, Integer) -> Integer
encrypt secret (key, base) = secret^key `mod` base