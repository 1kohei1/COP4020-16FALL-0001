-- regular expression

infixl 8 <|>
infixl 9 <.>

type RegExp = String -> Bool

epsilon :: RegExp
epsilon = (== "") -- operator section

char :: Char -> RegExp
char c = (== [c])

(<|>) :: RegExp -> RegExp -> RegExp
(<|>) e1 e2 = \s -> e1 s || e2 s 

splits :: [a] -> [([a],[a])]
splits xs = map (flip splitAt xs) [0..length xs]

-- concatenation .
(<.>) :: RegExp -> RegExp -> RegExp
(<.>) e1 e2 =
  \s -> or [e1 prefix && e2 suffix | (prefix,suffix) <- splits s]

(<..>) :: RegExp -> RegExp -> RegExp
(<..>) e1 e2 =
  \s -> or [e1 prefix && e2 suffix | (prefix,suffix) <- drop 1 (splits s)]

star :: RegExp -> RegExp
star e = epsilon <|> (e <..> star e)

-- extra functions

letter :: RegExp               -- if you put $ then you don't have
                               -- to put parenthesis around ['a' ... 'Z']
letter = foldl1 (<|>) (map char $ ['a'..'z'] ++ ['A'..'Z'])

a = char 'a'
b = char 'b'

-- Our answer
-- option
option :: RegExp -> RegExp
option e = epsilon <|> e

-- plus
plus :: RegExp -> RegExp
plus e = e <..> star e


number :: RegExp
number = num1_9 <..> star num0_9 <|> zero

num0_9 :: RegExp
num0_9 = foldl1 (<|>) (map char $ ['0'..'9'])

num1_9 :: RegExp
num1_9 = foldl1 (<|>) (map char $ ['1'..'9'])

fractional :: RegExp
fractional = (number <|> zero) <..> dot <..> fraction

dot :: RegExp
dot = char '.'

zero :: RegExp
zero = char '0'

fraction :: RegExp
fraction = (star num0_9) <..> num1_9