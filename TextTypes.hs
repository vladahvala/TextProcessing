-- | Типи для подання тексту: Символ, Слово, Розділовий знак, Речення, Текст
module TextTypes
    ( Symbol (..)
    , Word (..)
    , Punctuation (..)
    , Element (..)
    , Sentence (..)
    , Text (..)
    , isTerminal
    , wordLength
    , sentenceWords
    , renderWord
    , renderElement
    , renderSentence
    , renderText
    ) where

import Prelude hiding (Word)
import Data.Char (isAlphaNum)

-- | Символ
newtype Symbol = Symbol { symbolChar :: Char }
    deriving (Eq, Show)

-- | Слово — послідовність символів
newtype Word = Word { wordSymbols :: [Symbol] }
    deriving (Eq, Show)

-- | Розділовий знак
newtype Punctuation = Punctuation { punctChar :: Char }
    deriving (Eq, Show)

-- | Елемент речення: слово, розділовий знак або пробіл
data Element
    = WordEl Word
    | PunctEl Punctuation
    | Space
    deriving (Eq, Show)

-- | Речення (пропозиція) — послідовність елементів
newtype Sentence = Sentence { sentenceElements :: [Element] }
    deriving (Eq, Show)

-- | Текст — послідовність речень
newtype Text = Text { textSentences :: [Sentence] }
    deriving (Eq, Show)

-- | Розділовий знак, що завершує речення
isTerminal :: Punctuation -> Bool
isTerminal (Punctuation c) = c `elem` ".!?…"

-- | Довжина слова — кількість букв і цифр
-- (апостроф і дефіс у словах типу "п'ять", "будь-який" не рахуються)
wordLength :: Word -> Int
wordLength = length . filter (isAlphaNum . symbolChar) . wordSymbols

-- | Усі слова речення
sentenceWords :: Sentence -> [Word]
sentenceWords (Sentence es) = [w | WordEl w <- es]

------------------------------------------------------------
-- Перетворення назад у рядок
------------------------------------------------------------

renderWord :: Word -> String
renderWord = map symbolChar . wordSymbols

renderElement :: Element -> String
renderElement (WordEl w)  = renderWord w
renderElement (PunctEl p) = [punctChar p]
renderElement Space       = " "

renderSentence :: Sentence -> String
renderSentence = concatMap renderElement . sentenceElements

-- | Речення розділяються одним пробілом
renderText :: Text -> String
renderText = unwords . map renderSentence . textSentences
