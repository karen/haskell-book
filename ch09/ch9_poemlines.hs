module PoemLines where

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines "" = []
myLines x = takeWhile (/= '\n') x : myLines (drop 1 $ dropWhile (/= '\n') x)

shouldEqual =
      [ "Tyger Tyger, burning bright"
      , "In the forests of the night"
      , "What immortal hand or eye"
      , "Could frame thy fearful symmetry?"
      ]

-- The main function here is a small test -- to ensure you've written your function -- correctly.
main :: IO ()
main =
    print $ "Are they equal? "
            ++ show (myLines sentences == shouldEqual)