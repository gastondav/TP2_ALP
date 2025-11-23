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
conversion t = conversion_aux [] t

conversion_aux :: [String] -> LamTerm -> Term
conversion_aux xs (LVar x) = ligada x xs 0 
conversion_aux xs (LAbs x t lterm) = Lam t (conversion_aux (x:xs) lterm)  
conversion_aux xs (LApp v u) = (conversion_aux xs v) :@: (conversion_aux xs u)
conversion_aux xs (LLet x t1 t2) = Let (conversion_aux xs t1) (conversion_aux (x:xs) t2)
conversion_aux xs LZero = Zero
conversion_aux xs (LSuc t) = Suc (conversion_aux xs t) 
conversion_aux xs (LRec t1 t2 t3) = Rec (conversion_aux xs t1) (conversion_aux xs t2) (conversion_aux xs t3)

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
sub i r Zero = Zero
sub i r (Suc t) = Suc (sub i r t)
sub i r (Rec t1 t2 t3) = Rec (sub i r t1) (sub i r t2) (sub i r t3)

-- convierte un valor en el término equivalente
quote :: Value -> Term
quote (VLam t f) = Lam t f 
quote (VNum nv) = quoteNum nv

quoteNum :: NumVal -> Term
quoteNum NZero     = Zero
quoteNum (NSuc nv) = Suc (quoteNum nv)
  

-- evalúa un término en un entorno dado
eval :: NameEnv Value Type -> Term -> Value
eval nvs (Free x) = buscarEnv nvs x 
eval nvs (Lam t term) = (VLam t term)
eval nvs (Let t1 t2) = let v1 = eval nvs t1 
                       in eval nvs (sub 0 (quote v1) t2)
eval nvs (u :@: v) = let f = eval nvs u
                         arg = eval nvs v
                         in aplicar f arg
                      where 
                        aplicar (VLam t cuerpo) arg = eval nvs (sub 0 (quote arg) cuerpo)
eval nvs Zero = VNum NZero
eval nvs (Suc t) = case eval nvs t of 
                    VNum nv -> VNum (NSuc nv)
eval nvs (Rec t1 t2 t3) = case eval nvs t3 of 
                          VNum NZero -> eval nvs t1
                          VNum (NSuc nv) -> let t_n = quote (VNum nv)
                                                t_rec = Rec t1 t2 t_n 
                                                apli = t2 :@: t_rec :@: t_n 
                                            in eval nvs apli

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
infer' c e (Let t1 t2) = infer' c e t1 >>= \type1 -> infer' (type1 : c) e t2
infer' c e Zero = ret NatT
infer' c e (Suc t) = infer' c e t >>= \tipo -> if tipo == NatT then ret tipo 
                                                               else matchError NatT tipo
infer' c e (Rec t1 t2 t3) = infer' c e t1 >>= \tipo1 -> infer' c e t2 >>= \tipo2 -> infer' c e t3 >>= \tipo3 -> 
  case tipo2 of 
    FunT tipoA (FunT NatT tipoB) -> if tipo1 == tipoA && tipo3 == NatT && tipoA == tipoB
                                    then ret tipoA
                                    else err "Tipos incorrectos en Rec: t1:T, t2:T->Nat->T, t3:Nat"
    _  -> err "El segundo argumento de R debe tener tipo T -> Nat -> T"
