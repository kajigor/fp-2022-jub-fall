{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
module Lambda where
import Data.Foldable
import Control.Monad.State
import Data.Char (chr, ord)

-- Тип для лямбда-термов.
-- Параметризуем типом переменной.
data Lambda a = Var a
              | App (Lambda a) (Lambda a)
              | Abs a (Lambda a)

class MyShow a where
  myShow :: a -> String

instance {-# OVERLAPS #-} MyShow String where
  myShow = id

instance {-# OVERLAPPABLE #-} Show a => MyShow a where
  myShow = show

instance MyShow a => Show (Lambda a) where
  show (Var x) = myShow x
  show (App x y) = showAppFirst x ++ " " ++ showAppSecond y
    where
      showAppFirst x@(Abs _ _) = "(" ++ show x ++ ")"
      showAppFirst x = show x

      showAppSecond x@(Var _) = show x
      showAppSecond x = "(" ++ show x ++ ")"
  show (Abs x y) = "\\" ++ myShow x ++ "." ++ show y

-- ДеБрауновское представление лямбда-термов
data DeBruijn = VarDB Int
              | AbsDB DeBruijn
              | AppDB DeBruijn DeBruijn
  deriving (Eq)

-- Красивая печать без лишних скобок.
instance Show DeBruijn where
  show (VarDB x) = show x
  show (AppDB x y) = showAppFirst x ++ " " ++ showAppSecond y
    where
      showAppFirst x@(AbsDB _) = "(" ++ show x ++ ")"
      showAppFirst x = show x

      showAppSecond x@(VarDB _) = show x
      showAppSecond x = "(" ++ show x ++ ")"
  show (AbsDB x) = "\\ " ++ show x

-- λx. λy. x ≡ λ λ 2
-- λx. λy. λz. x z (y z) ≡ λ λ λ 3 1 (2 1)
-- λz. (λy. y (λx. x)) (λx. z x) ≡ λ (λ 1 (λ 1)) (λ 2 1)

-- Преобразовать обычные лямбда-термы в деБрауновские
toDeBruijn :: Eq a => Lambda a -> DeBruijn
toDeBruijn x = evalState (go x []) 1
  where
    go :: Eq a => Lambda a -> [(a, Int)] -> State Int DeBruijn
    go (Var x) boundVarIndices = do
      let varAndIndex = find (\p -> fst p == x) boundVarIndices
      case varAndIndex of
        Just (_, i) -> do
          return (VarDB i)
        Nothing -> do
          nextFreeVar <- get
          put (nextFreeVar + 1)
          return (VarDB nextFreeVar)
    go (App x y) boundVarIndices = do
      xDB <- go x boundVarIndices
      yDB <- go y boundVarIndices
      return (AppDB xDB yDB)
    go (Abs x f) boundVarIndices = do
      let withoutX = filter (\p -> fst p /= x) boundVarIndices
      let plusOne = map (\p -> (fst p, 1 + snd p)) withoutX
      let newBoundVarIndices = (x, 1) : plusOne
      nextFreeVar <- get
      put (nextFreeVar + 1)
      fDB <- go f newBoundVarIndices
      resultNextFreeVar <- get
      put (resultNextFreeVar - 1)
      return (AbsDB fDB)

-- Преобразовать деБрауновские лямбда-термы в обычные.
fromDeBruijn :: DeBruijn -> Lambda Int
fromDeBruijn x = go x 0
  where
    go :: DeBruijn -> Int -> Lambda Int
    go (VarDB x) depth = Var (depth - x)  -- A negative index means a free variable
    go (AppDB x y) depth = App (go x depth) (go y depth)
    go (AbsDB f) depth = Abs depth (go f (depth + 1))


-- Проверка термов на альфа-эквивалентность.
alphaEq :: Eq a => Lambda a -> Lambda a -> Bool
alphaEq x y =
    go x y []
  where
    go :: Eq a => Lambda a -> Lambda a -> [(a, a)] -> Bool
    go (Var x) (Var y) firstToSecondMap =
      ((x, y) `elem` firstToSecondMap) || (x == y && all (doesNotContain x y) firstToSecondMap)
    go (App a b) (App c d) firstToSecondMap =
      go a c firstToSecondMap && go b d firstToSecondMap
    go (Abs x f) (Abs y g) firstToSecondMap =
      go f g ((x, y) : filter (doesNotContain x y) firstToSecondMap)
    go _ _ _ = False

    doesNotContain :: Eq a => a -> a -> (a, a) -> Bool
    doesNotContain x y p = fst p /= x && snd p /= y

-- Выберите подходящий тип для подстановок.
class FreshValues a where
  values :: [a]

instance FreshValues Int where
  values = [1..]

instance FreshValues String where
  values = map (\c -> [c]) ['a'..'z']

data Subst a = Subst a (Lambda a)

-- Capture-avoiding substitution.
cas :: FreshValues a => Lambda a -> Subst a -> Lambda a
cas = undefined

-- Возможные стратегии редукции (о них расскажут 7 ноября).
data Strategy = CallByValue | CallByName | NormalOrder | ApplicativeOrder

-- Интерпретатор лямбда термов, учитывающий стратегию.
eval :: Strategy -> Lambda a -> Lambda a
eval = undefined
