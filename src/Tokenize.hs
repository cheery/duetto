module Tokenize (tokenize, Token(..), Pos, Bundle) where

import qualified Data.Set as Set
import Data.Set (Set, fromList)
import Control.Applicative
import Control.Monad (guard)

tokenize :: String -> Either String [(Token, Pos, Pos)]
tokenize s = case untok start (s,1,1) of
    Just (tokens, ("",_,_)) -> Right tokens
    Just (tokens, (_,x,y))  -> Left $ "tokenizer couldn't pass col " ++ show x ++ " line " ++ show y
    Nothing                 -> Left $ "tokenizer couldn't parse anything"

keywords :: Set String
keywords = fromList [
    "^", "~", "≠", "<", "≤", "=", "≥", ">", "¬", "|", "-", ",", ";", ":",
    "/", ".", "(", ")", "[", "]", "{", "}", "*", "+", "←", "→", "∃", "⊓",
    "⊔", "∘", "∧", "↦", "⟨", "⋆", "◊", "⇒", "≡", "⟧", "⟦", "⟩", "⟯", "⟮",
    "∨", "class", "codata", "data", "def", "export", "import",
    "is", "record", "using", "where", "λ", "." ]

type Bundle = (String, Int, Int)
newtype Tokenizer a = Tok (Bundle -> Maybe (a, Bundle))

untok :: Tokenizer a -> (Bundle -> Maybe (a, Bundle))
untok (Tok a) = a

instance Functor Tokenizer where
    fmap f (Tok g) = Tok (fmap (\(x,y) -> (f x, y)) . g)

instance Applicative Tokenizer where
    pure a = Tok (\sxy -> Just (a, sxy))
    Tok f <*> Tok x = Tok (\sxy -> do (f', sxy') <- f sxy
                                      (x', sxy'') <- x sxy'
                                      pure (f' x', sxy''))

instance Monad Tokenizer where
    Tok x >>= f = Tok (\sxy -> do (x', sxy') <- x sxy
                                  untok (f x') sxy' )

instance Alternative Tokenizer where
    empty = Tok (\sxy -> Nothing)
    Tok x <|> Tok y = Tok (\sxy -> x sxy <|> y sxy)

getch :: Tokenizer Char
getch = Tok getch'
    where getch' :: Bundle -> Maybe (Char, Bundle)
          getch' ([],x,y) = empty
          getch' ((c:cs),x,y) | (c == '\n') = pure (c, (cs, 1, y+1))
          getch' ((c:cs),x,y)               = pure (c, (cs, x+1, y))

type Pos = (Int, Int)

getpos :: Tokenizer Pos
getpos = Tok getpos'
    where getpos' :: Bundle -> Maybe (Pos, Bundle)
          getpos' (cs,x,y) = pure ((x,y), (cs,x,y))

start :: Tokenizer [(Token, Pos, Pos)]
start = do
    many' space
    pos <- getpos
    tok <- optional' token
    pos' <- getpos
    case tok of
        Nothing -> pure []
        Just tok -> ((tok, pos, pos'):) <$> start

space = do
    sp <- getch
    if sp == '#'
    then untilNewline
    else guard (sp `Set.member` my_spaces)

untilNewline :: Tokenizer ()
untilNewline = do
    ch <- optional' getch
    case ch of
        Just '\n' -> pure ()
        Just _    -> untilNewline
        Nothing -> pure ()

my_spaces :: Set Char
my_spaces = fromList " \n\t\r"

data Token = TInteger Integer
           | TString String
           | TKeyword String
           | TId String
           | TReal String
           | TEnd
           deriving (Show, Eq)

token :: Tokenizer Token
token = digits <|> string <|> onecharkw <|> word

digits :: Tokenizer Token
digits = do
    digits <- some' digit
    c <- optional' (exact '.')
    case c of
        Just () -> do
            digits' <- some' digit
            pure (TReal (digits ++ "." ++ digits'))
        Nothing -> pure (TInteger (read digits))

digit = do
    ch <- getch
    guard (ch `Set.member` my_digits)
    pure ch

my_digits :: Set Char
my_digits = fromList "0123456789"

string :: Tokenizer Token
string = do
    exact '"'
    s <- many' char
    exact '"'
    pure (TString s)

char :: Tokenizer Char
char = do
    ch <- getch
    if ch == '\\'
    then getch
    else do guard (ch /= '"' || ch /= '\n')
            pure ch

onecharkw = do
    ch <- getch
    guard ([ch] `Set.member` keywords)
    pure (TKeyword [ch])

word :: Tokenizer Token
word = do
    ch <- letter
    chs <- many' (letter <|> digit)
    if (ch:chs) `Set.member` keywords
    then pure (TKeyword (ch:chs))
    else pure (TId (ch:chs))

letter :: Tokenizer Char
letter = do
    ch <- getch
    guard (not ([ch] `Set.member` keywords))
    guard (not (ch `Set.member` my_digits))
    guard (not (ch `Set.member` my_spaces))
    guard (ch /= '"')
    guard (ch /= '#')
    pure ch

exact :: Char -> Tokenizer ()
exact ch = do ch' <- getch
              guard (ch == ch')

optional' :: Tokenizer a -> Tokenizer (Maybe a)
optional' (Tok x) = Tok (\sxy ->
    case x sxy of
        Nothing -> pure (Nothing, sxy)
        xs -> fmap foo xs)
    where foo :: (a, Bundle) -> (Maybe a, Bundle)
          foo (a, sxy) = (Just a, sxy)

some' :: Tokenizer a -> Tokenizer [a]
some' p = do
    x <- p
    optional' (some' p) >>= maybe
        (pure [x])
        (pure . (x:))

many' :: Tokenizer a -> Tokenizer [a]
many' p = do
    optional' (some' p) >>= maybe
        (pure [])
        pure
