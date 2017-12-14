module Solution where

import Data.Either
import Types

type Context = [(Symbol, Type)]

myLookup :: Symbol -> Context -> Either String Type
myLookup x context = case lookup x context of
  Just t -> Right t
  Nothing -> Left $ "undefined variable " ++ show x

extend :: (Symbol, Type) -> Context -> Context
extend x context = x : context

typeOf :: Term -> Either String Type

typeOf = typeOf' []
  where
    typeOf' context t = case t of

      Sym x -> myLookup x context

      Lam x type1 t -> case typeOf' (extend (x, type1) context) t of
        Right type2 -> Right $ Fun type1 type2
        _ -> Left "wrong type in lambda"

      App t1 t2 -> case type1 of
        Right (Fun type1' type2') | Right type1' == type2 -> Right type2'
                                    | otherwise -> Left "App args wrong type"
        _ -> Left "wrong type in first term of application"
        where
          type1 = typeOf' context t1
          type2 = typeOf' context t2

      Boolean _ -> Right Bool

      Not t | typeOf' context t /= Right Bool -> Left "argument of Not must be boolean"
            | otherwise -> Right Bool

      And t1 t2 | type1 /= Right Bool -> Left "first argument of And must be boolean"
                | type2 /= Right Bool -> Left "second argument of And must be boolean"
                | otherwise -> Right Bool
                  where
                    type1 = typeOf' context t1
                    type2 = typeOf' context t2

      Or t1 t2 | type1 /= Right Bool -> Left "first argument of Or must be boolean"
               | type2 /= Right Bool -> Left "second argument of Or must be boolean"
               | otherwise -> Right Bool
                  where
                    type1 = typeOf' context t1
                    type2 = typeOf' context t2

      Iff t1 t2 t3 | type1 /= Right Bool -> Left "condition guard must be boolean"
                   | type2 /= t3Type -> Left "condition arms must have the same type"
                   | otherwise -> type2
                      where
                        type1 = typeOf' context t1
                        type2 = typeOf' context t2
                        t3Type = typeOf' context t3

      Natural n -> if n >= 0 then Right Nat else Left "must be nat"

      Add t1 t2 | type1 /= Right Nat -> Left "first argument of Add must be nutural number"
                | type2 /= Right Nat -> Left "second argument of Add must be nutural number"
                | otherwise -> Right Nat
                where
                  type1 = typeOf' context t1
                  type2 = typeOf' context t2

      Mult t1 t2 | type1 /= Right Nat -> Left "first argument of Mult must be nutural number"
                 | type2 /= Right Nat -> Left "second argument of Mult must be nutural number"
                 | otherwise -> Right Nat
                 where
                   type1 = typeOf' context t1
                   type2 = typeOf' context t2

      Pair t1 t2 -> case typeOf' context t1 of
        Left _ -> Left "pair first element wrong type"
        Right ty1 -> case typeOf' context t2 of
          Left _ -> Left "pair second element wrong type"
          Right ty2 -> Right $ PairT ty1 ty2

      Fst t | isLeft type' -> Left "Fst argument wrong type"
            | otherwise -> type'
            where
              type' = typeOf' context t

      Snd t | isLeft type' -> Left "Snd argument wrong type"
            | otherwise -> type'
            where
              type' = typeOf' context t
      Nil t -> Right $ List t

      IsNil t -> case typeOf' context t of
        Right (List _) -> Right Bool
        _ -> Left "argument of IsNil must be list"

      Head t -> case typeOf' context t of
        Right (List t') -> Right t'
        _ -> Left "argument of Head must be list"

      Tail t -> case typeOf' context t of
        Right (List t') -> Right $ List t'
        _ -> Left "argument of Tail must be list"

      Cons t1 t2 -> case typeOf' context t2 of
        Right (List type2) -> case typeOf' context t1 of
          (Right type1) | type2 == type1 -> Right $ List type1
                         | type1 /= type2 -> Left "first argument of Cons must have the same type as the elements of the second argument"
                         | otherwise -> Right $ List type1
          _ -> Left "wrong type of first argument of Cons"
        _ -> Left "second argument of Cons must be list"