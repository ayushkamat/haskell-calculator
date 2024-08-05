import qualified Data.Map (Map, empty, insert, lookup)
import Debug.Trace (trace)
import Text.Read (readMaybe)

isExit :: String -> Bool
isExit x = x == "exit" || x == "exit()"

isOperator :: Char -> Bool
isOperator x = x `elem` ['+', '-', '*', '/', '=', '(', ')']

isWhitespace :: Char -> Bool
isWhitespace x = x `elem` [' ', '\t', '\n']

toDoubleMaybe :: String -> Maybe Double
toDoubleMaybe x = readMaybe x :: Maybe Double

isValidAssignmentTarget :: String -> Bool
isValidAssignmentTarget x = not (null x) && notElem (head x) ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']

tokenize :: [String] -> String -> String -> [String]
tokenize finished cur rest =
  if null rest
    then finished ++ [cur]
    else tokenizeHelper finished cur rest

tokenizeHelper :: [String] -> String -> String -> [String]
tokenizeHelper finished cur rest
  | isOperator next = tokenize (finished ++ if cur == "" then [[next]] else [cur, [next]]) "" (tail rest)
  | isWhitespace next = tokenize finished cur (tail rest)
  | otherwise = tokenize finished (cur ++ [next]) (tail rest)
  where
    next = head rest

data AST a
  = Empty
  | Literal a
  | Identifier a
  | Unary a (AST a)
  | Binary a (AST a) (AST a)
  | Assign a (AST a)
  deriving (Show, Read, Eq)

parseGrouping :: (AST String, [String]) -> (AST String, [String])
parseGrouping (initial, tokens)
  | null tokens = (initial, tokens)
  | head tokens == "(" =
    let (sub, next) = parseExpression (Empty, tail tokens)
     in if head next == ")" then (sub, tail next) else error "unterminated ("
  | otherwise = case toDoubleMaybe (head tokens) of
    Just _ -> (Literal (head tokens), tail tokens)
    Nothing -> if isValidAssignmentTarget (head tokens) then (Identifier (head tokens), tail tokens) else (initial, tokens)

parseUnary :: (AST String, [String]) -> (AST String, [String])
parseUnary (initial, tokens)
  | null tokens = (initial, tokens)
  | head tokens == "-" =
    let (sub, next) = parseGrouping (Empty, tail tokens)
     in (Unary (head tokens) sub, next)
  | otherwise = parseGrouping (initial, tokens)

parseMul :: (AST String, [String]) -> (AST String, [String])
parseMul (initial, tokens)
  | null rest = (left, rest)
  | head rest `elem` ["*", "/"] =
    let (right, next) = parseUnary (Empty, tail rest)
     in parseMul (Binary (head rest) left right, next)
  | otherwise = (left, rest)
  where
    (left, rest) = parseUnary (initial, tokens)

parseAdd :: (AST String, [String]) -> (AST String, [String])
parseAdd (initial, tokens)
  | null rest = (left, rest)
  | head rest `elem` ["+", "-"] =
    let (right, next) = parseMul (Empty, tail rest)
     in parseAdd (Binary (head rest) left right, next)
  | otherwise = (left, rest)
  where
    (left, rest) = parseMul (initial, tokens)

parseAssign :: (AST String, [String]) -> (AST String, [String])
parseAssign (initial, tokens)
  | null tokens = (initial, tokens)
  | length tokens > 2 = case head (tail tokens) of
    "=" ->
      if isValidAssignmentTarget (head tokens)
        then
          let (right, next) = parseAssign (Empty, tail (tail tokens))
           in parseExpression (Assign (head tokens) right, next)
        else error "invalid assignment target"
    _ -> parseAdd (initial, tokens)
  | otherwise = parseAdd (initial, tokens)

parseExpression :: (AST String, [String]) -> (AST String, [String])
parseExpression (x, rest) = parseAssign (x, rest)

toAST :: [String] -> (AST String, [String])
toAST tokens = parseExpression (Empty, tokens)

evalAST :: (AST String, Data.Map.Map String Double) -> (Double, Data.Map.Map String Double)
evalAST (ast, env) = case ast of
  Assign id sub ->
    let (res, new) = evalAST (sub, env)
     in (res, Data.Map.insert id res new)
  Unary op sub ->
    let (res, new) = evalAST (sub, env)
     in if op == "-" then (-1 * res, new) else error ("unknown unary expression operation " ++ op)
  Binary op left right ->
    let (leftRes, new) = evalAST (left, env)
     in let (rightRes, newer) = evalAST (right, new)
         in case op of
              "+" -> (leftRes + rightRes, newer)
              "-" -> (leftRes - rightRes, newer)
              "*" -> (leftRes * rightRes, newer)
              "/" -> (leftRes / rightRes, newer)
              _ -> error ("unknown binary operation " ++ op)
  Literal x -> case toDoubleMaybe x of
    Just y -> (y, env)
    Nothing -> error ("non double literal: " ++ x)
  Identifier x -> case Data.Map.lookup x env of
    Just res -> (res, env)
    Nothing -> error ("no such variable in scope: " ++ x)
  _ -> error "unknown expression type"

repl :: Data.Map.Map String Double -> IO ()
repl env = do
  line <- getLine
  if isExit line
    then return ()
    else
      let (res, new) = evalAST (fst (toAST (tokenize [] "" line)), env)
       in do
            print res
            repl new

main = repl Data.Map.empty
