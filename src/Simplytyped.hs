module Simplytyped
  ( conversion
  ,    -- conversion a terminos localmente sin nombre
    eval
  ,          -- evaluador
    infer
  ,         -- inferidor de tipos
    quote          -- valores -> terminos
  )
where

import           Data.List
import           Data.Maybe
import           Prelude                 hiding ( (>>=) )
import           Text.PrettyPrint.HughesPJ      ( render )
import           PrettyPrinter
import           Common

-----------------------
-- conversion
-----------------------

-- conversion a términos localmente sin nombres
conversion :: LamTerm -> Term
conversion (LVar x) = (Free (Global x))
conversion (LAbs x tipo lterm) = Lam tipo (conversion_aux [x] lterm )
conversion (LApp v u) = (conversion v) :@: (conversion u)


conversion_aux :: [String] -> LamTerm -> Term
conversion_aux xs (LVar x) = (ligada x xs 0) 
conversion_aux xs (LAbs x t lterm) = Lam t (conversion_aux (x:xs) lterm)  
conversion_aux xs (LApp v u) = (conversion_aux xs v) :@: (conversion_aux xs u)

ligada :: String -> [String] -> Int -> Term
ligada name [] _ = (Free (Global name))
ligada name (x:xs) i = if x == name 
                        then (Bound i)
                        else ligada name xs (i + 1)

----------------------------
--- evaluador de términos
----------------------------

-- substituye una variable por un término en otro término
sub :: Int -> Term -> Term -> Term
sub i t (Bound j) | i == j    = t
sub _ _ (Bound j) | otherwise = Bound j
sub _ _ (Free n   )           = Free n
sub i t (u   :@: v)           = sub i t u :@: sub i t v
sub i t (Lam t'  u)           = Lam t' (sub (i + 1) t u)

-- convierte un valor en el término equivalente
quote :: Value -> Term
quote (VLam t f) = Lam t f

-- evalúa un término en un entorno dado
eval :: NameEnv Value Type -> Term -> Value
eval nvs (Free x) = buscarEnv nvs x 
eval nvs (Lam t term) = (VLam t term)
eval nvs (u :@: v) = let f = eval nvs u
                         arg = eval nvs v
                         in aplicar f arg
                      where 
                        aplicar (VLam t cuerpo) arg = eval nvs (sub 0 (quote arg) cuerpo)

buscarEnv :: NameEnv Value Type -> Name -> Value
buscarEnv ((y, (valor, tipo)):xs) x = if x == y 
                                      then valor
                                      else buscarEnv xs x


----------------------
--- type checker
-----------------------

-- infiere el tipo de un término
infer :: NameEnv Value Type -> Term -> Either String Type
infer = infer' []

-- definiciones auxiliares
ret :: Type -> Either String Type
ret = Right

err :: String -> Either String Type
err = Left

(>>=)
  :: Either String Type -> (Type -> Either String Type) -> Either String Type
(>>=) v f = either Left f v
-- fcs. de error

matchError :: Type -> Type -> Either String Type
matchError t1 t2 =
  err
    $  "se esperaba "
    ++ render (printType t1)
    ++ ", pero "
    ++ render (printType t2)
    ++ " fue inferido."

notfunError :: Type -> Either String Type
notfunError t1 = err $ render (printType t1) ++ " no puede ser aplicado."

notfoundError :: Name -> Either String Type
notfoundError n = err $ show n ++ " no está definida."

-- infiere el tipo de un término a partir de un entorno local de variables y un entorno global
infer' :: Context -> NameEnv Value Type -> Term -> Either String Type
infer' c _ (Bound i) = ret (c !! i)
infer' _ e (Free  n) = case lookup n e of
  Nothing     -> notfoundError n
  Just (_, t) -> ret t
infer' c e (t :@: u) = infer' c e t >>= \tt -> infer' c e u >>= \tu ->
  case tt of
    FunT t1 t2 -> if (tu == t1) then ret t2 else matchError t1 tu
    _          -> notfunError tt
infer' c e (Lam t u) = infer' (t : c) e u >>= \tu -> ret $ FunT t tu


