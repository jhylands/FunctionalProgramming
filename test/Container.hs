import System.Environment
import Prelude hiding (pure)
import Parse
import Pretty
import Data.Char (isUpper, isAlpha) 

data Decl  =  Decl Name [Cons]
              deriving Show

data Cons  =  Cons Name [Arg]
              deriving Show

data Arg   =  VarA | AppA Name
              deriving Show

type Name  =  String

-- These parser components may be useful.

newLine :: Parser Char
newLine  =  char '\n'

space :: Parser Char
space  =  char ' '

indent :: Parser ()
indent  =  () ... many space ..* many (newLine ..* many1 space)  

symbol  :: String -> Parser String
symbol s  =  string s ..* indent

decl :: Parser Decl
decl  =  error "Declare a working 'decl' parser. See Q2(a)."

declsDoc :: [Decl] -> DOC
declsDoc  =  error "Declare a working 'declsDoc'. See Q2(b)."

prettyDecls :: Int -> [Decl] -> String
prettyDecls w ds  =  pretty w (declsDoc ds)

class Size c where
  size :: c t -> Int

data List t  =  Nil | Snoc (List t) t

instance Size List where
  size Nil          =  0
  size (Snoc xs _)  =  size xs + 1

data CatTree t  =  One t | Cat (CatTree t) (CatTree t)

-- Add Size instances for Maybe and CatTree.  See Q2(c).

sizeInstance :: Decl -> String
sizeInstance  =  error "Declare a working 'sizeInstance'. See Q2(d)."

main  =  do src <- getContents
            let Just ds  =  parseWith (many decl) src
            [w] <- getArgs
            putStrLn (prettyDecls (read w) ds)
            mapM_ (putStr . sizeInstance) ds


