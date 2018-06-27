--ParseProgramSecondaParte.hs

module ParseProgramSecondaParte where
import Control.Applicative
import Control.Monad
import Data.Char
import System.IO

-----Definizioni
-- A core-language program is just a list of supercombinatoric definitions
type Program a = [ScDefn a]
type CoreProgram = Program Name

-- a supercombinatoric definition contains the name of the supercombinator, its arguments and its body
type ScDefn a = (Name,[a],Expr a)
type CoreScDefn = ScDefn Name

type Name = String

type Def a = (a, Expr a) -- for let and letrec
type Alter a = (Int, [a], Expr a) -- for case

-- in Expr using IsRec you use the constructor ELet for modelling both let and letrec
data IsRec = NonRecursive | Recursive
             deriving Show

data Expr a = EVar Name
            | ENum Int
            | EConstr Int Int
            | EAp (Expr a) (Expr a)
            | ELet
                IsRec
                [Def a]
                (Expr a)
            | ECase
                (Expr a)
                [Alter a]
            | ELam [a] (Expr a)
            deriving Show

-- parole-chiave non utilizzabili come nomi di variabili o funzioni
keywords = ["of", "let", "letrec", "in", "case","Pack"] 
------------------------------------------

-----Definizione Functor, Applicative e Monad per Parser
newtype Parser a = P (String -> [(a,String)])

instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap g p = P (\inp -> case parse p inp of
        [] -> []
        [(v,out)] -> [(g v, out)])

instance Applicative Parser where
    -- pure :: a -> Parser a
    pure v = P (\inp -> [(v,inp)])

    -- <*> :: Parser (a -> b) -> Parser a -> Parser b
    pg <*> px = P (\inp -> case parse pg inp of
        [] -> []
        [(g,out)] -> parse (fmap g px) out)

instance Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = P (\inp -> case parse p inp of
        [] -> []
        [(v,out)] -> parse (f v) out)

-- "sostitisce" il costruttore dummy P
parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp

--------------------------------------------------------------------------------

-- it fails if input string is empty. Otherwhise succeeds with the first Char as the result value
item :: Parser Char
item = P (\inp -> case inp of
    [] -> []
    (x:xs) -> [(x,xs)])

instance Alternative Parser where
    -- empty :: Parser a
    empty = P (\inp -> [])

    -- (<|>) :: Parser a -> Parser a -> Parser a
    p <|> q = P (\inp -> case parse p inp of
              [] -> parse q inp
              [(v,out)] -> [(v,out)])

-- ritorna un parser solo se c e' vero
-- parse (sat (=='a')) "abc"  =  [('a',"bc")]
sat :: (Char -> Bool) -> Parser Char
sat c = do x <- item
           if (c x) 
               then return x 
               else empty

digit :: Parser Char -- cifre numeriche
digit = sat isDigit

lower :: Parser Char -- lettere minuscole
lower = sat isLower

upper :: Parser Char -- lettere minuscole
upper = sat isUpper

letter :: Parser Char -- caratteri
letter = sat isAlpha

alphanum :: Parser Char -- parole alphanumeriche
alphanum = sat isAlphaNum

char :: Char -> Parser Char 
char x = sat (== x)

-- return a parser iff the string is at the beginning of the param
-- parse (string "ab") "abc"  =  [("ab","c")]
-- parse (string "ab") "aab"  =  []
string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

-- parse an identifier (variable name)
-- parse ident "width = 10"  =  [("width"," = 10")]
ident :: Parser String
ident = do {x <- lower;
           xs <- many alphanum;
		   if (elem (x:xs) keywords)
				then empty;
				else return (x:xs);}

-- parse a natural number
-- parse nat "18 * 23"  =  [(18," * 23")]
nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

-- parse a space
space :: Parser ()
space = do many (sat isSpace)
           return ()

-- parse an integer
int :: Parser Int 
int = do char '-'
         n <- nat
         return (-n)
    <|> nat

--------------------------------------------------------------------------------
-- Gestione degli 'Spazi'
-- permette di ignorare gli spazi prima e dopo l'applicazione di un parse per un certo token
token :: Parser a -> Parser a
token p = do space
             v <- p 
             space 
             return v

-- per 'parsare' un nome evitando gli spazi prima e dopo
identifier :: Parser String
identifier = token ident

-- per 'parsare' un naturale evitando gli spazi
natural :: Parser Int
natural = token nat

-- per 'parsare' un intero evitando gli spazi
integer :: Parser Int
integer = token int

-- per 'parsare' un naturale evitando gli spazi
symbol :: String -> Parser String
symbol xs = token (string xs)

-- parser per progamma
parseProg :: Parser (Program Name)
parseProg = do p <- parseScDef
               do symbol ";"
                  ps <- parseProg
                  return (p:ps)
                  <|> return [p]

-- parser per supercombinator
parseScDef :: Parser (ScDefn Name)
parseScDef = do v <- identifier
                pf <- many identifier
                char '='
                body <- parseExpr
                return (v, pf, body)
                
-- expr puo' appartenere ad uno dei seguenti casi: let, letrec, case, lambda and aexpr.
-- ognuno ha una funzione dedicata: parseLet, parseLetRec, parseCase, parseLambda, parseAExpr
parseExpr :: Parser (Expr Name)
parseExpr = parseLet <|> parseLetRec <|> parseCase <|> parseLambda <|> parseExpr1 -- <|> parseAExpr

parseLet :: Parser (Expr Name) -- "let defns in expr"
parseLet = do symbol "let"
              defns <- some parseDef 
              symbol "in"
              e <- parseExpr
              return (ELet NonRecursive defns e)

parseLetRec :: Parser (Expr Name) -- "letrec defns in expr"
parseLetRec = do symbol "letrec"
                 defns <- some parseDef
                 symbol "in"
                 body <- parseExpr
                 return (ELet Recursive defns body)

parseCase :: Parser (Expr Name) -- "case expr of alts"
parseCase = do symbol "case"
               e <- parseExpr
               symbol "of"
               alts <- parseAlts
               return (ECase e alts)

parseLambda :: Parser (Expr Name) -- "\ var1 ... varn . expr "
parseLambda = do symbol "\\"
                 var <- some identifier
                 symbol "."
                 e <- parseExpr
                 return (ELam var e)

-- AExpr riguardano casi partiolari come variabili, numeri, costruttori, parentesi
parseAExpr :: Parser (Expr Name)
parseAExpr = parseVar <|> parseNum <|> parsePack <|> parseParenthesisedExpr

parseVar :: Parser (Expr Name) -- "var"
parseVar = do var <- identifier
              return (EVar var)

parseNum :: Parser (Expr Name) -- "num"
parseNum = do num <- integer
              return (ENum num)     

parsePack :: Parser (Expr Name) -- "Pack{num,num}"
parsePack = do symbol "Pack"
               symbol "{"
               tag <- integer
               symbol ","
               arity <- integer
               symbol "}"
               return (EConstr tag arity)

parseParenthesisedExpr :: Parser (Expr Name) -- "(expr)"
parseParenthesisedExpr = do symbol "("
                            e <- parseExpr
                            symbol ")"
                            return e

parseAlts :: Parser [(Alter Name)] -- "alt1; ...; altn"
parseAlts = do alt <- parseAlt
               do symbol ";"
                  remaining_alts <- parseAlts
                  return (alt:remaining_alts)
                  <|>
                  return [alt]

-- parser per il singolo alt, ritorna una tripla con numero, variabili ed espressione
parseAlt :: Parser (Alter Name) -- "<num> var1 ... varn -> expr"
parseAlt = do symbol "<"
              num <- integer
              symbol ">"
              var <- many identifier
              symbol "->"
              e <- parseExpr
              return (num, var, e)


-- Parse a "Definition". It's used by parseLet and parseLetRec for Def (let and letrec).
parseDef :: Parser (Def Name)
parseDef = do x <- identifier
              symbol "="
              expr <- parseAExpr
              return (x, expr)

----------------SECONDA PARTE-----------------------------

parseExpr1 :: Parser (Expr Name)
parseExpr1 = do left <- parseExpr2
                do symbol "|"
                   right <- parseExpr1
                   return (EAp (EAp (EVar "|") left) right)
                   <|> 
                   return left 

parseExpr2 :: Parser (Expr Name)
parseExpr2 = do left <- parseExpr3
                do symbol "&"
                   right <- parseExpr2
                   return (EAp (EAp (EVar "&") left) right)
                   <|>
                   return left

parseExpr3 :: Parser (Expr Name)
parseExpr3 = do left <- parseExpr4
                do relop <- parseRelOp
                   right <- parseExpr4
                   return (EAp(EAp (EVar relop) left) right) 
                   <|>
                   return left

parseRelOp :: Parser Name
parseRelOp = symbol "==" <|> symbol "~=" <|> symbol ">" <|> symbol ">=" <|> symbol "<" <|> symbol "<"

parseExpr4 :: Parser (Expr Name)
parseExpr4 = do left <- parseExpr5
                do symbol "+"
                   right <- parseExpr4
                   return (EAp (EAp (EVar "+") left) right)
                   <|>
                   do symbol "-"
                      right <- parseExpr5
                      return (EAp (EAp (EVar "-") left) right)
                      <|> 
                      return left

parseExpr5 :: Parser (Expr Name)
parseExpr5 = do left <- parseExpr6
                do symbol "*"
                   right <- parseExpr5
                   return (EAp (EAp (EVar "*") left) right)
                 <|> do symbol "/"
                        right <- parseExpr6
                        return (EAp(EAp (EVar "/") left) right)
                 <|> return left

parseExpr6 :: Parser (Expr Name)
parseExpr6 = do el <- parseAExpr
                do x <- parseExpr6
                   return (EAp el x)
                 <|> return el

