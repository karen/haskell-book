largestPrimeFact' d num ans =
    case d * d <= num of
        True -> if num `mod` d == 0
            then largestPrimeFact' (d) (num `div` d) (d)
            else largestPrimeFact' (d+1) (num) (ans)
        False -> if num > ans then num else ans

largestPrimeFact num = largestPrimeFact' 2 num 1