module Parse where
import Text.Parsec
import Syntax
import Data.Functor (($>))
import Control.Applicative ((<**>), empty)
import Text.Parsec.Expr (buildExpressionParser, Operator (Postfix, Prefix, Infix), Assoc (AssocLeft))
import Data.Bifunctor (first)
import Data.Function ((&))

type Parser = Parsec String ()

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

args :: Parser a -> Parser [a]
args x = parens $ try (word x) `sepBy` char ','

targs :: Parser a -> Parser [a]
targs x = between (char '<') (char '>') $ try (word x) `sepBy` char ','

braces :: Parser a -> Parser a
braces = between (char '{') (char '}')

word :: Parser a -> Parser a
word = between spaces spaces

name :: Parser Name
name = (:) <$> letter <*> many alphaNum

upperName :: Parser Name
upperName = (:) <$> upper <*> many alphaNum

lowerName :: Parser Name
lowerName = (:) <$> lower <*> many alphaNum

decl :: Parser Decl
decl = do
    try $ string "effect"
    n <- word name
    t <- targs name
    e <- word $ targs name
    ess <- targs name
    spaces
    es <- braces $ spaces >> effect `endBy1` spaces
    pure . Interface $ Interf n (Bind t e ess) es
    <|> Function <$> fun

fun :: Parser Fun
fun = Fun <$> name <*> word (args arg) <*> body

arg :: Parser Arg
arg = Arg <$> name <*> optionMaybe (try (word $ char ':') >> ty)

body :: Parser Body
body = braces $ spaces >> stmt `endBy` spaces

effect :: Parser Effect
effect = Effect <$> name <*> word (args ty) <* char ':' <* spaces <*> ty

expr :: Parser Expr
expr = expr1 >>= \e -> fmap (foldl Handle e) . many . try $ word (string "with") *> expr1

expr1 :: Parser Expr
expr1 = expr0 >>= \e -> fmap (foldl (&) e) . many . try $
    flip . flip Call <$>
        word (option 0 $ char '@' *>
            option 1 (read <$> many1 digit)) <*>
        args expr

expr0 :: Parser Expr
expr0 = try (string "fun") $> Lambda <*> word (args arg) <*> body
    <|> try (string "handler") $> Handler <*> word lowerName <*> braces (spaces >> fun `endBy1` spaces)
    <|> Lit <$> lit
    <|> EffCall <$> upperName <* spaces <*> args expr
    <|> Var <$> lowerName
    <|> parens (word expr)

lit :: Parser Lit
lit =
    try (string "null") $> Unit ()
    <|> char '"' $> Str <*> (char '\\' *> escape <|> anyChar) `manyTill` char '"'
    <|> do
    m <- option "" $ string "-"
    xs <- many1 digit
    option (Num . read $ m <> xs) $ do
        d <- option "" $ string "."
        ys <- many1 digit
        pure . Frac . read $ m <> xs <> d <> ys
    where
        escape = anyChar >>= maybe empty pure . (`lookup` zip "\\nt" "\\\n\t")

stmt :: Parser Stmt
stmt =  try (string "if") $> IfS <*> word expr <*> body <*>
        option [ExprS . Lit $ Unit ()] (try (word $ string "else") *> body)
    <|> try (string "while") $> WhileS <*> word expr <*> body
    <|> try (LetS <$> name <* word (char '=')) <*> expr
    <|> ExprS <$> expr

ty :: Parser Type
ty =    try (string "fun") $> FunT <*> word (args ty) <*> effs <* spaces <*> ty
    <|> try (string "handler") *> word (parens . word $ HandlerT <$> eff <* spaces <*> ty)
        <*> effs <* spaces <*> ty
    <|> VarT <$> lowerName
    <|> ConT <$> upperName

eff :: Parser Eff
eff =   Eff <$> upperName <*> word (targs ty) <*> targs eff <* spaces <*> targs effs
    <|> EffVar <$> lowerName

effs :: Parser Effs
effs = between (char '[') (char ']') $ Effs <$> word eff `sepBy` char ',' <* char '|' <*> word name

program :: Parser [Decl]
program = decl `endBy` spaces

parseProgram :: String -> Either String [Decl]
parseProgram = first show . parse (program <* eof) "<SOURCE>"
