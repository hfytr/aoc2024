main :: IO()
main = putStrLn (greet "World")

greet :: String -> String
greet who = "Hello" ++ who
