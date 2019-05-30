{-Пусть А - последовательность букв. После вычеркивания одной буквы из А (в одной позиции)получили последовательность В. После вычеркивания другой буквы из А (в одной позиции) получили последовательность С.
Можно ли по последовательностям B и С
1) Определить вычеркнутые буквы.
2) Определить последовательность A.
Примечание: В и C могут быть получены вычеркиванием одной и той же буквы.-}

{-Проверка возможности выполнить пункт 1, т.е. определить вычеркнутые буквы-}
{-За одно проверяем, что строки правильные, т.е. одной длинны-}
checkChars :: String -> String -> Bool
checkChars b c = (b /= c {-Если не равны, значит вычеркнули разные символы-}) && (length b == length c) {-Проверка правильности строк-}

{-Здесь проверяем возможность определения исходной строки A-}
checkRecovery :: String -> String -> Bool
checkRecovery b c = let
                        checkStep b c = let
                                            i = findIndex b c 0 {-Ищем позицию первого несоответствия между B и C-}
                                        in
                                            if (i == length b - 1 {-Если несоответствие в последнем символе,-})
                                                                  {-восстановить точно не сможем-}
                                             then False else {-Иначе проверяем, что между вычеркнутыми символами был промежуток-}
                                                             {-Именно благодаря промежутку мы можем восстановить строку A.-}
                                                (b!!i == c!!(i + 1)) || (b!!(i + 1) == c!!i)
                    in
                        checkStep b c {-Применяем проверку-}

{-Поиск позиции несоответствия-}
findIndex :: String -> String -> Int -> Int
findIndex b c i = if (if max (length b - 1) (length c - 1) == i {-Если это конец строки, возвращаем позицию последнего символа-} 
                                                                then False else 
                                                                    {-Иначе сравниваем симолы из B и C.-}
                                                                    (b!!i) == (c!!i)) then findIndex b c (i + 1) else i

{-Здесь выполняется пункт 1 задания, т.е. ищем вычеркнутые символы-}
{-Возвращает массив с элементами следующего вида:-}
{-(Позиция в строке A, (Где отсутствует B-1; C-2, Какой символ))-}
getChars :: String -> String -> [(Int, (Int, Char))]
getChars b c = let
                {-Определяем в какой строке вычеркнут символ по длине совпадающего участка-}
                {-Следующие две функции и находят данную длинну-}
                getEqLen1 b c i acc = if (i == length b - 1) then acc else if b!!i == c!!(i + 1) then getEqLen1 b c (i + 1) (acc + 1) else acc
                getEqLen2 b c i acc = if (i == length b - 1) then acc else if b!!(i + 1) == c!!i then getEqLen2 b c (i + 1) (acc + 1) else acc
                {-Определяем в какой строке отсутствует симол и какой.-}
                getIntChar b c i = if max (length b - 1) (length c - 1) == i then (if length b > length c then (2, b!!i) else (1, c!!i))
                                    else if (getEqLen1 b c i 0) > (getEqLen2 b c i 0) then (1, c!!i) else (2, b!!i)
                {-Ищем символы-}
                findChars b c = let
                                    i = findIndex b c 0 {-Позиция первого несоответствия-}
                                    ch = getIntChar b c i {-Первый символ-}
                                    {-Дополняем наши строки найденным символом-}
                                    {-Если отсутствует в B, добавляем B, иначе не изменяем-}
                                    nb = if fst ch == 1 then (take i b ++ [snd ch] ++ drop i b) else b
                                    {-Со строкой C тоже самое-}
                                    nc = if fst ch == 2 then (take i c ++ [snd ch] ++ drop i c) else c
                                    i2 = findIndex nb nc i {-Позиция второго несоответствия на дополненных строках-}
                                    ch2 = getIntChar nb nc i2 {-Второй символ-}
                                in
                                    [(i, ch),(i2, ch2)] {-Возвращаем найденные символы-}
               in
                findChars b c {-Вызываем поиск символов-}

{-Восстанавливающая функция-}
recovery :: String -> String -> (Int, (Int, Char)) -> String
recovery b c ch = let 
                {-Дополняем одну из строк первым найденным символом-}
                {-В зависимости от того, где его не хватает-}
                nb = if fst (snd ch) == 1 then (take (fst ch) b ++ [snd (snd ch)] ++ drop (fst ch) b) else b
                nc = if fst (snd ch) == 2 then (take (fst ch) c ++ [snd (snd ch)] ++ drop (fst ch) c) else c
               in
                if length nb > length nc then nb else nc {-Возвращаем дополненную строку.-}

main = do
    putStrLn "Введите строку B:" {-Просим ввести строку-}
    b <- getLine {-Сохраняем-}
    putStrLn "Введите строку C:" {-Повторяем для второй строки-}
    c <- getLine
    if checkChars b c then do {-Проверяем возможность выполнения пункта 1 задания.-}
        let chars = getChars b c {-Если можем выполнить, ищем символы-}
        let ch1 = chars!!0 {-И привязываем их к отдельным функциям-}
        let ch2 = chars!!1
        putStrLn "Найдены вычеркнутые буквы:"
        if checkRecovery b c then do {-Проверяем возможность восстановления-}
            let a = recovery b c ch1 {-Если возможно, восстанавливаем-}
            let ib = (if fst (snd ch1) == 1 then fst ch1 else fst ch2) + 1 {-Также определаем, где было совершено вычёркивание-}
            let ic = (if fst (snd ch1) == 2 then fst ch1 else fst ch2) + 1 {--}
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
