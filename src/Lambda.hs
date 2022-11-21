{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant where" #-}
module Lambda where
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe (isNothing)

-- Тип для лямбда-термов.
-- Параметризуем типом переменной.
data Lambda a = Var a
              | App (Lambda a) (Lambda a)
              | Abs a (Lambda a)
              deriving (Eq)

-- true ≡ λx.λy.x
true = Abs "x" (Abs "y" (Var "x"))

-- false ≡ λx.λy.y
false = Abs "x" (Abs "y" (Var "y"))

-- and ≡ λp.λq.p q p
and = Abs "p" (Abs "q" (App (App (Var "p") (Var "q")) (Var "p")))

-- or ≡ λp.λq.p p q
or = Abs "p" (Abs "q" (App (App (Var "p") (Var "p")) (Var "q")))

-- not ≡ λp.p FALSE TRUE
not = Abs "p" (App (App (Var "p") false) true)

-- ifThenElse ≡ λp.λa.λb.p a b
ifThenElse = Abs "p" (Abs "a" (Abs "b" (App (App (Var "p") (Var "a")) (Var "b"))))

-- zero ≡ λf.λx.x
zero = Abs "f" (Abs "x" (Var "x"))

-- one ≡ λf.λx.f x
one = Abs "f" (Abs "x" (App (Var "f") (Var "x")))

-- two ≡ λf.λx.f (f x)
two = Abs "f" (Abs "x" (App (Var "f") (App (Var "f") (Var "x"))))

-- three ≡ λf.λx.f (f (f x))
three =  Abs "f" (Abs "x" (App (Var "f") (App (Var "f") (App (Var "f") (Var "x")))))

--  four ≡ λf.λx.f (f (f (f x)))
four =  Abs "f" (Abs "x" (App (Var "f") (App (Var "f") (App (Var "f") (App (Var "f") (Var "x"))))))

-- add ≡ λm.λn.λf.λx.m f (n f x)
add = Abs "m" (Abs "n" (Abs "f" (Abs "x" (App (App (Var "m") (Var "f")) (App (App (Var "n") (Var "f")) (Var "x"))))))

-- successor ≡ λn.λf.λx.f (n f x)
successor = Abs "n" (Abs "f" (Abs "x" (App (Var "f") (App (App (Var "n") (Var "f")) (Var "x")))))

-- add' ≡ λm.λn.m successor n
add' = Abs "m" (Abs "n" (App (App (Var "m") successor) (Var "n")))

-- mult ≡ λm.λn.λf.m (n f)
mult = Abs "m" (Abs "n" (App (Var "m") (App (Var "n") (Var "f"))))

-- mult' ≡ λm.λn.m (add n) 0
mult' = Abs "m" (Abs "n" (App (App (Var "m") (App add (Var "n"))) zero))

commonShow :: Lambda a -> (a -> String) -> String
commonShow (Var x) f = f x
commonShow (App (Abs x l1) (App l r)) f = "(" ++ commonShow (Abs x l1) f ++ ") " ++ "(" ++ commonShow (App l r) f ++ ")"
commonShow (App (Abs x l1) l2) f = "(" ++ commonShow (Abs x l1) f ++ ") " ++ commonShow l2 f
commonShow (App l1 (App l2 l3)) f = commonShow l1 f ++ " (" ++ commonShow (App l2 l3) f ++ ")"
commonShow (App l r) f = commonShow l f ++ " " ++ commonShow r f
commonShow (Abs l r) f = "\\" ++ f l ++ "." ++ commonShow r f

-- Красивая печать без лишних скобок.
instance {-# OVERLAPS #-} Show (Lambda String) where
  show l = commonShow l id

-- data Lambda a = Var a
--               | App (Lambda a) (Lambda a)
--               | Abs a (Lambda a)

instance {-# OVERLAPPABLE #-} Show a => Show (Lambda a) where
  show l = commonShow l show

-- Выберите подходящий тип для подстановок.
data Subst a = Subst a (Lambda a) (Map.Map a a) -- переменная, заменитель, (map, которые рашает, на что заменять свободные переменные земенители при необходимости)
                                                -- значения в Map подразумеваются отличными от всех переменных в лямбде, в которой заменяем
-- Проверка термов на альфа-эквивалентность.
alphaEq :: Ord a => Ord b => Lambda a -> Lambda b -> Bool -- Ord по причинам описанным в toDeBruijn
alphaEq l r = toDeBruijn l == toDeBruijn r

-- Множество свободных переменных 
fv :: Ord a => Lambda a -> Set.Set a
fv (Var x) = Set.singleton x
fv (App l r) = Set.union (fv l) (fv r)
fv (Abs x l) = Set.delete x (fv l)

-- Capture-avoiding substitution.
cas :: Ord a => Lambda a -> Subst a -> Lambda a
cas (Var x) (Subst v s _) | x == v = s
                          | otherwise = Var x
cas (App l r) subst = App (cas l subst) (cas r subst)
cas (Abs x l) (Subst v s m) | x == v = Abs x l
                            | Set.notMember x (fv s) = Abs x (cas l (Subst v s m))
                            | otherwise = Abs (m Map.! x) (cas (cas l (Subst x (Var (m Map.! x)) Map.empty)) (Subst v s m))
                            -- можно передавать Map.empty, из-за предположения в 86 строчке
    

-- Возможные стратегии редукции (о них расскажут 7 ноября).
data Strategy = CallByValue | CallByName | NormalOrder | ApplicativeOrder

-- Интерпретатор лямбда термов, учитывающий стратегию.
eval :: Strategy -> Lambda a -> Lambda a
eval = undefined

-- ДеБрауновское представление лямбда-термов
data DeBruijn = VarDB Int
              | AbsDB DeBruijn
              | AppDB DeBruijn DeBruijn
              deriving (Eq)

-- Красивая печать без лишних скобок.
instance Show DeBruijn where
  show (VarDB x) = show x
  show (AppDB (AbsDB l1) (AppDB l r)) = "(" ++ show (AbsDB l1) ++ ") " ++ "(" ++ show (AppDB l r) ++ ")"
  show (AppDB (AbsDB l1) l2) = "(" ++ show (AbsDB l1) ++ ") " ++ show l2
  show (AppDB l1 (AppDB l2 l3)) = show l1 ++  " (" ++ show (AppDB l2 l3) ++ ")"
  show (AppDB l r) = show l ++ " " ++ show r
  show (AbsDB r) = "\\ " ++ show r

-- λx. λy. x ≡ λ λ 2
-- λx. λy. λz. x z (y z) ≡ λ λ λ 3 1 (2 1)
-- λz. (λy. y (λx. x)) (λx. z x) ≡ λ (λ 1 (λ 1)) (λ 2 1)


fst3 :: (a, b, c) -> a -- не получилось почему-то заимпортить (не знаю почему)
fst3 (a, _, _) = a

-- Преобразовать обычные лямбда-термы в деБрауновские
toDeBruijn :: Ord a => Lambda a -> DeBruijn -- выбор: либо (Ord a) и Map, либо (Hashable a) и HashMap 
                                            -- ну или хранить все в листе и делать все за квадрат
                                            -- (если без Map, то застрелиться можно)
toDeBruijn l = fst3 $ getDeBruijn l 0 Map.empty Map.empty 1
  where -- Можно было бы бахнуть State монаду, кажется (но ее писать долго, быстрее это)
    getDeBruijn :: Ord a => Lambda a -> Int -> Map.Map a Int -> Map.Map a Int -> Int -> (DeBruijn, Map.Map a Int, Int)
    getDeBruijn (Var x) depth m freeMap freeDepth | isNothing (Map.lookup x m) = if isNothing (Map.lookup x freeMap) then  -- Свободная переменная
                                                                                    let freeMap1 = Map.insert x (freeDepth - 1) freeMap in
                                                                                      (VarDB (depth - (freeMap1 Map.! x) + 1), freeMap1, freeDepth - 1)
                                                                                 else 
                                                                                      (VarDB (depth - (freeMap Map.! x) + 1), freeMap, freeDepth)
                                                  | otherwise = (VarDB (depth - (m Map.! x) + 1), freeMap, freeDepth)
    getDeBruijn (Abs x rest) depth m freeMap freeDepth = 
      let (deb1, freeMap1, freeDepth1) = getDeBruijn rest (depth + 1) (Map.insert x (depth + 1) m) freeMap freeDepth in
        (AbsDB deb1, freeMap1, freeDepth1)
    getDeBruijn (App l r) depth m freeMap freeDepth =
      let (deb1, freeMap1, freeDepth1) = getDeBruijn l depth m freeMap freeDepth in
        let (deb2, freeMap2, freeDepth2) = getDeBruijn r depth m freeMap1 freeDepth1 in
          (AppDB deb1 deb2, freeMap2, freeDepth2)

-- add ≡ λm.λn.λf.λx.m f (n f x)
-- add = Abs "m" (Abs "n" (Abs "f" (Abs "x" (App (App (Var "m") (Var "f")) (App (App (Var "n") (Var "f")) (Var "x"))))))

-- Преобразовать деБрауновские лямбда-термы в обычные.
fromDeBruijn :: DeBruijn -> Lambda Int
fromDeBruijn d = fst3 $ fromDB d 0 0 Map.empty Map.empty
  where -- тоже, наверное, по-хорошему надо было State монадой делать
    fromDB :: DeBruijn -> Int -> Int -> Map.Map Int Int -> Map.Map Int Int -> (Lambda Int, Int, Map.Map Int Int)
    fromDB (VarDB x) depth lastID m freeMap | isNothing (Map.lookup (depth - x + 1) m) =  if isNothing (Map.lookup (depth - x + 1) freeMap) then -- Свободная переменная
                                                                                            let freeMap1 = Map.insert (depth - x + 1) (lastID + 1) freeMap in
                                                                                              (Var (freeMap1 Map.! (depth - x + 1)), lastID + 1, freeMap1)
                                                                                          else 
                                                                                              (Var (freeMap Map.! (depth - x + 1)), lastID, freeMap)
                                            | otherwise = (Var (m Map.! (depth - x + 1)), lastID, freeMap)
    fromDB (AbsDB rest) depth lastID m freeMap = 
      let (l, lastID1, freeMap1) = fromDB rest (depth + 1) (lastID + 1) (Map.insert (depth + 1) (lastID + 1) m) freeMap in 
        (Abs (lastID + 1) l, lastID1, freeMap1)
    fromDB (AppDB l r) depth lastID m freeMap = 
      let (curL, lastID1, freeMap1) = fromDB l depth lastID m freeMap in
        let (curR, lastID2, freeMap2) = fromDB r depth lastID1 m freeMap1 in
          (App curL curR, lastID2, freeMap2)
