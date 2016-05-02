{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module Data.Flist where

data Flist (p :: k -> *) (c :: [k]) where
    Nil  :: Flist p '[]
    (:.) :: p a -> Flist p as -> Flist p (a ': as)

infixr 5 :.

flistMap :: (forall a. f a -> g a) -> Flist f b -> Flist g b
flistMap f list = case list of
    h :. t -> f h :. flistMap f t
    Nil    -> Nil

flistMapAccum :: (forall a. f a -> acc -> (g a, acc)) -> acc -> Flist f b -> (Flist g b, acc)
flistMapAccum f acc list = case list of
    h :. t -> let (h', acc')  = f h acc
                  (t', acc'') = flistMapAccum f acc' t in (h' :. t', acc'')
    Nil    -> (Nil, acc)

flistMapFold :: Monoid m => (forall a. f a -> (g a, m)) -> Flist f b -> (Flist g b, m)
flistMapFold f = flistMapAccum (\v acc -> let (v', acc') = f v in (v', acc `mappend` acc')) mempty
