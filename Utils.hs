module Utils where

split :: [a] -> ([a], [a])
split []       = ([], [])
split [x]      = ([x], [])
split (x:y:xs) = ( x : (fst (split xs)), y : (snd (split xs)) )

split3 l = (xs, ys, zs)
  where (xs, qs) = split l
        (ys, zs) = split qs
        
-- ��������� ���
intToInteger :: Int -> Integer
intToInteger x = read (show x)

-- ��� ��������� ��� 8)
intToFloat :: Int -> Float
intToFloat x = read (show x)

