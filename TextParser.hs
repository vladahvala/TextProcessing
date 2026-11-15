-- | Розбір рядка на речення, слова та розділові знаки
module TextParser
    ( normalizeSpaces
    , tokenize
    , splitSentences
    , trimSpaces
    , collapseSpaces
    , normalizeElements
    , parseText
    ) where

import Prelude hiding (Word)
import Data.Char (isAlphaNum, isDigit, isSpace)
import TextTypes

-- | Заміна табуляцій, переходів рядка та послідовностей пробілів одним пробілом
normalizeSpaces :: String -> String
normalizeSpaces = unwords . words

-- | Символ, з якого може складатися слово
isWordChar :: Char -> Bool
isWordChar c = isAlphaNum c || c == '_'

-- | Символи, що з'єднують частини одного слова: п'ять, будь-який
isJoiner :: Char -> Bool
isJoiner c = c `elem` "'’ʼ-"

-- | Розбиття рядка на елементи: слова, розділові знаки, пробіли
tokenize :: String -> [Element]
tokenize [] = []
tokenize s@(c:cs)
    | isSpace c    = Space : tokenize (dropWhile isSpace cs)
    | isWordChar c =
        let (w, rest) = spanWord s
        in WordEl (Word (map Symbol w)) : tokenize rest
    | otherwise    = PunctEl (Punctuation c) : tokenize cs

-- | Виділяє слово з початку рядка.
-- Апостроф і дефіс між буквами та крапка між цифрами (3.14) належать слову.
spanWord :: String -> (String, String)
spanWord (a:rest@(b:c:_))
    | isWordChar a && isJoiner b && isWordChar c = consume
    | isDigit a && b == '.' && isDigit c         = consume
  where
    consume = let (w, r) = spanWord (drop 1 rest) in (a : b : w, r)
spanWord (a:rest)
    | isWordChar a = let (w, r) = spanWord rest in (a : w, r)
spanWord s = ([], s)

-- | Розбиття послідовності елементів на речення
splitSentences :: [Element] -> [Sentence]
splitSentences es =
    case dropWhile (== Space) es of
        [] -> []
        xs ->
            let (s, rest) = takeSentence xs
            in Sentence (trimSpaces s) : splitSentences rest

-- | Бере елементи до кінця речення.
-- Речення закінчується знаком . ! ? … (можливо, кількома поспіль і
-- закривною дужкою/лапкою), після якого йде пробіл або кінець тексту.
-- Тому "obj.method" або "3.14" не розривають речення.
takeSentence :: [Element] -> ([Element], [Element])
takeSentence [] = ([], [])
takeSentence (e : rest)
    | isEnd e =
        let (closing, after) = span isClosingOrTerminal rest
        in if startsBoundary after
               then (e : closing, after)
               else continue
    | otherwise = continue
  where
    continue = let (s, r) = takeSentence rest in (e : s, r)

isEnd :: Element -> Bool
isEnd (PunctEl p) = isTerminal p
isEnd _           = False

isClosingOrTerminal :: Element -> Bool
isClosingOrTerminal (PunctEl p@(Punctuation c)) =
    isTerminal p || c `elem` ")]}»\"'”"
isClosingOrTerminal _ = False

startsBoundary :: [Element] -> Bool
startsBoundary []          = True
startsBoundary (Space : _) = True
startsBoundary _           = False

-- | Прибирає пробіли на початку і в кінці
trimSpaces :: [Element] -> [Element]
trimSpaces = reverse . dropWhile (== Space) . reverse . dropWhile (== Space)

-- | Кілька пробілів поспіль замінює одним
collapseSpaces :: [Element] -> [Element]
collapseSpaces (Space : Space : rest) = collapseSpaces (Space : rest)
collapseSpaces (e : rest)             = e : collapseSpaces rest
collapseSpaces []                     = []

normalizeElements :: [Element] -> [Element]
normalizeElements = trimSpaces . collapseSpaces

-- | Розбиття на абзаци за порожніми рядками.
-- Порожній рядок теж завершує речення: так заголовок без крапки
-- не зливається з наступним реченням.
paragraphs :: String -> [String]
paragraphs = go . lines
  where
    isBlank = all isSpace
    go ls =
        case break isBlank (dropWhile isBlank ls) of
            ([], _)   -> []
            (p, rest) -> unwords p : go rest

-- | Повний розбір тексту
parseText :: String -> Text
parseText = Text . concatMap (splitSentences . tokenize . normalizeSpaces) . paragraphs
