
-- Takes a string, lowercases it, drops any character that is not a letter or a space, and returns a list with the words in the string.

-- > toWordList "Hello, World! HELLO!! :-)"
-- ["hello","hello","world"]


toWordList :: String -> [String]
toWordList = toWordList' [] ""

-- Auxiliar function for toWordList.
-- Takes a text and reads it character by character, accumulating letters to form words and saving this words and saving this words every time a space is found.

toWordList' :: [String] -> String -> String -> [String]
toWordList' wordsFound currentWord ""        = 
  if null currentWord 
    then wordsFound 
    else insertWord wordsFound currentWord

toWordList' wordsFound currentWord (ch:tail)
  | isLetter ch =
      toWordList' wordsFound (currentWord ++ [(lowerLetter ch)]) tail
  | isSpace  ch = 
      if null currentWord 
        then toWordList' wordsFound currentWord tail 
        else toWordList' (insertWord wordsFound currentWord) "" tail
  | otherwise   = toWordList' wordsFound currentWord tail
  where
    isLetter ch    = elem ch ['a'..'z'] || elem ch ['A'..'Z']
    isSpace ch     = elem ch "\t\n\v\f\r "
    lowerLetter ch = 
      if elem ch ['A'..'Z']
        then toEnum $ (fromEnum ch) + 32 
        else ch

-- Inserts a word in a list of words in a position besides another identical word, if there is one.

-- > insertWord ["of", "the", "apes"] "the"
-- ["of","the","the","apes"]

insertWord :: [String] -> String -> [String]
insertWord words str = (takeWhile (/= str) words) ++ [str] ++ (dropWhile (/= str) words)

-- Takes a list of strings and returns the number of times the 20 most frequently used English words appears in the list.

-- > countCommonWords ["the","planet","of","the","apes"]
-- 3

countCommonWords :: [String] -> Int
countCommonWords []     = 0
countCommonWords (w:tail) 
  | w `elem` eocList    = (countCommonWords tail) + 1
  | otherwise           = countCommonWords tail 

-- Takes a list of strings and drops any word that is within the top 20 most commonly used in English. Returns a list of strings without those words.

-- > dropCommonWords ["the","planet","of","the","apes"]
-- ["planet","apes"]

dropCommonWords :: [String] -> [String]
dropCommonWords []    = []
dropCommonWords (w:tail) 
  | notElem w eocList = w : dropCommonWords tail
  | otherwise         = dropCommonWords tail 

-- Takes a list of strings and returns a list. Each element of the returned list is a tuple which contains a string (a word) and an integer (representing the number of times the word appears in the text).

-- > countWords ["friend","she","she"]
-- [("friend",1),("she",2)]

countWords :: [String] -> [(String, Int)]
countWords []               = []
countWords (firstWord:tail) = reverse $ countWords' [] (firstWord, 1) tail

-- Auxiliar function for countWords
-- Groups each word together in a single word-occurrence tuple.

countWords' :: [(String, Int)] -> (String, Int) -> [String] -> [(String, Int)] 
countWords' wordsCounted currentWord []          = (currentWord : wordsCounted)
countWords' wordsCounted currentTuple@(currentWord, occurrences) (word:rest) 
  | word == currentWord = countWords' wordsCounted (currentWord, occurrences + 1) rest
  | otherwise           = countWords' (currentTuple:wordsCounted) (word, 1) rest

-- Sorts words by their frequency in descending order. It takes and returns a list of tuples. Each element of the tuple consists of one string (the word) and one integer (its frequency).

-- > sortWords [("friend",1),("she",2)]
-- [("she",2),("friend",1)]

sortWords :: [(String, Int)] -> [(String, Int)]
sortWords [] = []
sortWords ((w, n):tail) = (sortWords greater) ++ [(w, n)] ++ (sortWords lesser)
  where
    lesser  = [(x,m) | (x,m) <- tail, m < n || (m == n && compareAlphabetically x w) ]
    greater = [(x,m) | (x,m) <- tail, m > n || (m == n && compareAlphabetically w x) ]

-- Checks whether the first word should be before the second alphabetically

-- > compareAlphabetically "worst" "wisdom" 
-- False

compareAlphabetically :: String -> String -> Bool
compareAlphabetically "" "" = True
compareAlphabetically "" w2 = True
compareAlphabetically w1 "" = False
compareAlphabetically (ch1:tail1) (ch2:tail2) =
  if ch1 /= ch2 
    then ch1 < ch2
    else compareAlphabetically tail1 tail2

-- Makes a string representing histogram row using asterisks. It takes a tuple (string, integer) and returns a string consisting of a number of asterisks (the second element of the tuple), the string " -> " , the word (the first element of the tuple), and finally a newline ( " \n " ).

-- > makeHistogramRow ("test", 10)
-- "********** -> test\n

makeHistogramRow :: (String, Int) -> String
makeHistogramRow (w, n) = (take n (repeat '*')) ++ " -> " ++ w ++ "\n"

-- Makes a histogram using asterisks. It takes a list of tuples (string, integer) and returns a string which contains the histogram. This function should use the makeHistogramRow method to generate the string. Only the top 20 most frequent words should be shown.

-- > makeHistogram [("her",4),("she",2),("friend",1)]
-- "**** -> her\n** -> she\n* -> friend\n"

makeHistogram :: [(String, Int)] -> String
makeHistogram tuples = concatMap makeHistogramRow (take 20 tuples)

-- List of the top 20 most common words, ranked by EOC

eocList = ["the", "and", "have", "not", "as", "be", "a", "I", "on", "you", "to", "in", "it", "with", "do", "of", "that", "for", "he", "at"]

---------------------------------------------------------------------------
-- Main Code
---------------------------------------------------------------------------

text = "It was the best of times, it was the worst of times, it was the age of wisdom, it was the age of foolishness, it was the epoch of belief, it was the epoch of incredulity, it was the season of Light, it was the season of Darkness, it was the spring of hope, it was the winter of despair, we had everything before us, we had nothing before us, we were all going direct to Heaven, we were all going direct the other way--in short, the period was so far like the present period, that some of its noisiest authorities insisted on its being received, for good or for evil, in the superlative degree of comparison only.\nThere were a king with a large jaw and a queen with a plain face, on the throne of England; there were a king with a large jaw and a queen with a fair face, on the throne of France. In both countries it was clearer than crystal to the lords of the State preserves of loaves and fishes, that things in general were settled for ever."


main = do
    let wordlist = toWordList text
    putStrLn "Report:"
    putStrLn ("\t" ++ (show $ length wordlist) ++ " words")
    putStrLn ("\t" ++ (show $ countCommonWords wordlist) ++ " common words")
    putStrLn "\nHistogram of the most frequent words (excluding common words):\n"
    putStr $ makeHistogram $ sortWords $ countWords $ dropCommonWords $ wordlist