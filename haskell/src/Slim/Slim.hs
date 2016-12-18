{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE IncoherentInstances        #-}
{-# LANGUAGE OverlappingInstances       #-}
{-# LANGUAGE TypeSynonymInstances       #-}


module Slim.Slim where
import           Control.Monad
import           Data.List
import           Debug.Trace
import           Text.ParserCombinators.Parsec
import           Text.Printf

data Instruction id = Import id PackName
                 | Make  id Instance Class  [ Argument ]
                 | Call  id  Instance Method [ Argument ]
                 | CallAndAssign  id Instance Symbol Method [ Argument ]
                deriving (Eq,Show, Read)

type PackName     = String
type Instance = String
type Class    = String
type Method   = String
type Symbol   = String
type Argument = String
data Answer = A NestedList
            | KO String
           deriving (Eq, Show)
data NestedList = S String
                | L [ NestedList ]
                        deriving (Eq,Show)


class SlimEncodable a where
  encode :: a -> String


aLength l = printf "%06d" (length l)


instance SlimEncodable String where
  encode string = aLength string ++ ":" ++  string


instance SlimEncodable NestedList where
   encode (S s) = encode s
   encode (L l) = encode string
        where
           string = concat [
                             "["
                           , aLength l
                           , ":"
                           , (concatMap (( ++ ":") . encode) l)
                           , "]"
                           ]

instance SlimEncodable Answer where
  encode (A l)  = encode l
  encode (KO s) = encode ("__EXCEPTION__:" ++ s)

class SlimDecodable a where
  decode :: String -> Maybe a


instance SlimDecodable NestedList where
  decode s = case (runParser slimparser "" "" s) of
               Left e   -> trace (show e) Nothing
               Right ts -> Just ts


readInt :: String -> Int
readInt s = case reads s of
              (x,_):_ -> x
              e       -> error $ "cannot read int from " ++ show e



slimparser  =   do len <- lengthOfData ; char ':'
                   (listParser <|> stringParser len)
            where
	      contentOfLength n = count n anyChar
	      lengthOfData      = fmap readInt (count 6 digit)
	      manyStrings n     = count n (do nest <- slimparser; char ':'
	                                      return nest)
	      stringParser len  = fmap S (contentOfLength len)
	      listParser        = do char '['
	                      	     size    <- lengthOfData     ; char ':'
	                      	     strings <- manyStrings size ; char ']'
	                      	     return $ L strings

instance (SlimEncodable a) => SlimEncodable [ a ] where
  encode l  = encode' l "" where
      encode' []     l@(x:_)  = encode (l ++ "]")
      encode' (x:xs) l'@(y:_) = encode' xs (l' ++ encode x ++ ":")
      encode' l'        []    = encode' l' ("["
                                  ++ aLength l'
                                  ++ ":")


instance SlimEncodable (Instruction String) where
  encode (Import id path)
       = encode [ id , "import", path ]
  encode (Make id inst cls args)
       = encode ( [ id , "make", inst, cls ] ++ args)
  encode (Call id inst meth args)
       = encode ( [ id , "call", inst, meth ] ++ args)
  encode (CallAndAssign id symbol inst meth args)
       = encode ( [ id , "callAndAssign", symbol, inst, meth ] ++  args)

instance SlimDecodable [Instruction String] where
  decode s = let
               nest = decode s  :: Maybe NestedList
               fromNestedList :: [NestedList] -> [ String ]
               fromNestedList ((S s : rest)) = s : (fromNestedList rest)
               fromNestedList ([])           = []
               decode' x@(L [S id, S "import", S pack]) = trace (show x) (Import id pack)
               decode' x@(L (S id : S "make" : S inst:  S cls : args)) = trace (show x) (Make id inst cls (fromNestedList args))
               decode' x@(L (S id : S "call" : S inst:  S cls : args)) = trace (show x) (Call id inst cls (fromNestedList args))
               decode' x@(L (S id : S "callAndAssign" : S inst: S var :  S cls : args)) = trace (show x) (CallAndAssign id inst var cls (fromNestedList args))
               decode' x    = trace (show x) (Import "" "")
             in case nest of
               Just (L insts) -> Just (map decode' insts)
               Nothing        -> Nothing

instance SlimDecodable Answer where
  decode s = let
               nest = decode s  :: Maybe NestedList
             in case nest of
                 Just (S ('_':'_':'E':'X':'C':'E':'P':'T':'I':'O':'N':'_':'_':':': message ))
                    -> Just $ KO message
                 Just r -> Just $ A r
                 Nothing -> Nothing

idOf :: Instruction String -> String
idOf (Import         id _)       = id
idOf (Make           id _ _ _)   = id
idOf (Call           id _ _ _)   =  id
idOf (CallAndAssign  id _ _ _ _) = id

matchQandA :: [Instruction String] -> Maybe Answer -> [ (Instruction String, Answer)]
matchQandA insts  Nothing              = map (\ i -> (i , KO "No answer from client")) insts
matchQandA insts (Just e@(KO reason))  = map (\ i -> (i ,e)) insts
matchQandA insts (Just (A (L as)))    = [(q, a) | q <- insts,
                                                  a <- matchOneAnswer as q]

matchOneAnswer :: [ NestedList ] -> Instruction String -> [Answer]
matchOneAnswer (a@(L [S num, rep]): as) q | idOf q == num = [A rep]
                                          | otherwise     = matchOneAnswer as q
matchOneAnswer []                         q               = [KO "No answer from client"]

renumber :: [ Instruction String ] -> [ Instruction String]
renumber insts = renumberFrom 1 insts
    where
      renumberFrom n (x:xs) = setId x n : renumberFrom (n+1) xs
      renumberFrom _ []     = []
      setId :: Instruction a -> Int -> Instruction String
      setId  (Make _ id cls pars)               n = Make (show n) id cls pars
      setId (Call _ id ope pars)                n = Call (show n) id ope pars
      setId (CallAndAssign _ id label ope pars) n = CallAndAssign (show n) id label ope pars
      setId (Import _ scope)                    n = Import (show n) scope

isException :: String -> Answer -> Bool
isException exc (A (S msg)) = isPrefixOf ("__EXCEPTION__:" ++ exc) msg

maybeAnInt :: String -> Maybe Int
maybeAnInt s = case (reads :: String -> [(Int,String)]) s of
                 (x,_):_ -> Just x
                 _       -> Nothing
