-- | Лабораторна робота №3, варіант 16.
-- У деякому реченні тексту слова заданої довжини замінити вказаним
-- підрядком, довжина якого може не збігатися з довжиною слова.
module Main where

import Prelude hiding (Word)
import Control.Exception (IOException, evaluate, try)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import System.IO
import Text.Read (readMaybe)
import TextTypes
import TextParser

------------------------------------------------------------
-- Завдання
------------------------------------------------------------

-- | Замінює в реченні всі слова довжини len елементами підрядка sub
replaceWordsOfLength :: Int -> [Element] -> Sentence -> Sentence
replaceWordsOfLength len sub (Sentence es) =
    Sentence (normalizeElements (concatMap replace es))
  where
    replace (WordEl w) | wordLength w == len = sub
    replace e = [e]

-- | Виконує заміну в реченні з номером n (нумерація з 1)
replaceInText :: Int -> Int -> [Element] -> Text -> Text
replaceInText n len sub (Text ss) =
    let (before, rest) = splitAt (n - 1) ss
    in case rest of
        (s : after) -> Text (before ++ replaceWordsOfLength len sub s : after)
        []          -> Text ss

------------------------------------------------------------
-- Ввід / вивід
------------------------------------------------------------

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

askLine :: String -> IO String
askLine msg = do
    putStrLn msg
    hFlush stdout
    getLine

askInt :: String -> (Int -> Bool) -> IO Int
askInt msg ok = do
    s <- askLine msg
    case readMaybe s of
        Just v | ok v -> return v
        _ -> do
            putStrLn "Некоректне значення, спробуйте ще раз."
            askInt msg ok

-- | Читає файл у UTF-8 (з BOM або без)
readFileUtf8 :: FilePath -> IO String
readFileUtf8 path = withFile path ReadMode $ \h -> do
    hSetEncoding h utf8_bom
    s <- hGetContents h
    _ <- evaluate (length s)
    return s

writeFileWith :: TextEncoding -> FilePath -> String -> IO ()
writeFileWith enc path s = withFile path WriteMode $ \h -> do
    hSetEncoding h enc
    hPutStr h s

-- | Запитує шлях до файлу, доки файл не вдасться прочитати
askFile :: IO String
askFile = do
    p <- askLine "Шлях до файлу з текстом (Enter - textbook.txt):"
    let file = if null (trim p) then "textbook.txt" else trim p
    r <- try (readFileUtf8 file) :: IO (Either IOException String)
    case r of
        Right s -> return s
        Left e -> do
            putStrLn ("Не вдалося прочитати файл: " ++ show e)
            askFile

printNumbered :: (Int, Sentence) -> IO ()
printNumbered (i, s) =
    putStrLn (show i ++ ". " ++ renderSentence s
              ++ "  [слів: " ++ show (length (sentenceWords s)) ++ "]")

------------------------------------------------------------
-- Головна функція
------------------------------------------------------------

main :: IO ()
main = do
    -- UTF-8 для консолі; ROUNDTRIP не дає програмі впасти,
    -- якщо в консолі інше кодування
    enc <- mkTextEncoding "UTF-8//ROUNDTRIP"
    mapM_ (`hSetEncoding` enc) [stdin, stdout, stderr]

    putStrLn "=== Лабораторна робота №3, варіант 16 ==="
    raw <- askFile

    let text      = parseText raw
        sentences = textSentences text
        count     = length sentences

    if count == 0
        then putStrLn "У файлі немає жодного речення."
        else do
            putStrLn ("\nТекст (табуляції та зайві пробіли замінено одним пробілом), речень: "
                      ++ show count ++ "\n")
            mapM_ printNumbered (zip [1 ..] sentences)
            putStrLn ""

            n      <- askInt ("Номер речення (1-" ++ show count ++ "):")
                             (\x -> x >= 1 && x <= count)
            len    <- askInt "Довжина слів, які треба замінити:" (> 0)
            subStr <- askLine "Підрядок, яким замінювати слова:"

            let sub      = tokenize (normalizeSpaces subStr)
                newText  = replaceInText n len sub text
                oldS     = sentences !! (n - 1)
                newS     = textSentences newText !! (n - 1)
                found    = [renderWord w | w <- sentenceWords oldS, wordLength w == len]

            putStrLn "\nРечення до заміни:"
            putStrLn (renderSentence oldS)
            putStrLn ("Слова довжини " ++ show len ++ ": "
                      ++ (if null found then "немає" else unwords found))
            putStrLn ("Замінено слів: " ++ show (length found))
            putStrLn "Речення після заміни:"
            putStrLn (renderSentence newS)

            putStrLn "\nТекст після заміни:"
            putStrLn (renderText newText)

            writeFileWith enc "result.txt" (renderText newText ++ "\n")
            putStrLn "\nРезультат також записано у файл result.txt"
