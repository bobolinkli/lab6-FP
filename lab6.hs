checkChars :: String -> String -> Bool
checkChars b c = (b /= c) && (length b == length c)

checkRecovery :: String -> String -> Bool
checkRecovery b c = let
                        checkStep b c = let
                                            i = findIndex b c 0
                                        in
                                            if (i == length b - 1) then False else (b!!i == c!!(i + 1)) || (b!!(i + 1) == c!!i)
                    in
                        checkStep b c

findIndex :: String -> String -> Int -> Int
findIndex b c i = if (if max (length b - 1) (length c - 1) == i then False else (b!!i) == (c!!i)) then findIndex b c (i + 1) else i

getChars :: String -> String -> [(Int, (Int, Char))]
getChars b c = let
                getEqLen1 b c i acc = if (i == length b - 1) then acc else if b!!i == c!!(i + 1) then getEqLen1 b c (i + 1) (acc + 1) else acc
                getEqLen2 b c i acc = if (i == length b - 1) then acc else if b!!(i + 1) == c!!i then getEqLen2 b c (i + 1) (acc + 1) else acc
                getIntChar b c i = if max (length b - 1) (length c - 1) == i then (if length b > length c then (2, b!!i) else (1, c!!i))
                                    else if (getEqLen1 b c i 0) > (getEqLen2 b c i 0) then (1, c!!i) else (2, b!!i)
                findChars b c = let
                                    i = findIndex b c 0
                                    ch = getIntChar b c i
                                    nb = if fst ch == 1 then (take i b ++ [snd ch] ++ drop i b) else b
                                    nc = if fst ch == 2 then (take i c ++ [snd ch] ++ drop i c) else c
                                    i2 = findIndex nb nc i
                                    ch2 = getIntChar nb nc i2
                                in
                                    [(i, ch),(i2, ch2)]
               in
                findChars b c               

recovery :: String -> String -> (Int, (Int, Char)) -> String
recovery b c ch = let 
                nb = if fst (snd ch) == 1 then (take (fst ch) b ++ [snd (snd ch)] ++ drop (fst ch) b) else b
                nc = if fst (snd ch) == 2 then (take (fst ch) c ++ [snd (snd ch)] ++ drop (fst ch) c) else c
               in
                if length nb > length nc then nb else nc

main = do
    putStrLn "Введите строку B:"
    b <- getLine
    putStrLn "Введите строку C:"
    c <- getLine
    if checkChars b c then do
        let chars = getChars b c
        putStrLn (show chars)
        let ch1 = chars!!0
        let ch2 = chars!!1
        putStrLn "Найдены вычеркнутые буквы:"
        if checkRecovery b c then do
            let a = recovery b c ch1
            let ib = (if fst (snd ch1) == 1 then fst ch1 else fst ch2) + 1
            let ic = (if fst (snd ch1) == 2 then fst ch1 else fst ch2) + 1
            putStrLn ("В B это \"" ++ (if fst (snd ch1) == 1 then [snd (snd ch1)] else [snd (snd ch2)]) ++ "\" на " ++ (show ib) ++ " позиции строки A;")
            putStrLn ("В C это \"" ++ (if fst (snd ch1) == 2 then [snd (snd ch1)] else [snd (snd ch2)]) ++ "\" на " ++ (show ic) ++ " позиции строки A.")
            putStrLn "Восстановленная строка A:"
            putStrLn a
         else do
            let i = min (fst ch1) (fst ch2) + 1
            putStrLn ("Это \"" ++ (if fst (snd ch1) == 1 then [snd (snd ch1)] else [snd (snd ch2)]) ++ "\" на " ++ (show i) ++ " позиции в строке B;")
            putStrLn ("Это \"" ++ (if fst (snd ch1) == 2 then [snd (snd ch1)] else [snd (snd ch2)]) ++ "\" на " ++ (show i) ++ " позиции в строке C.")
            putStrLn "Восстановить строку A не получится."
     else do
        putStrLn "Вычеркнут один и тот же символ либо вычеркнуто больше одного за раз. Определить символ и восстановить строку A не получится."
