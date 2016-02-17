main = mapM_ putStrLn $ take 100 fizzbuzzes

fizzbuzzes = map (\(n, f, b) -> if null (f++b) then show n else f++b) xs
  where xs = zip3 [1..] (cycle ["","","Fizz"]) (cycle ["", "", "", "", "Buzz"])
