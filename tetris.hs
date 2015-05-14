
-- ブロックの種類を表すデータ(Eは空白、Gは壁)
data Block = I | O | S | Z | J | L | T | E | G deriving (Show, Eq)


-- テトリスのフィールド（壁込み）
type Field = [[Block]]


-- フィールドのサイズ
fieldSizeX = 10
fieldSizeY = 20


-- 初期化フィールドの返却
initField :: Field
initField = (replicate fieldSizeY mid) ++ [last]
    where   mid = [G] ++ (replicate fieldSizeX E) ++ [G]
            last = replicate (length mid) G


-- Blockを画面に表示する文字に置き換える
blockToChar :: Block -> Char
blockToChar block
    | block == G = '#'
    | block == E = ' '
    | otherwise  = '*'


-- フィールドの表示
showField :: Field -> IO()
showField f = do
    mapM_  putStrLn (map (\x -> foldr (\y acc -> (blockToChar y) : acc) "" x) f)



-- 座標
type Pos = (Int, Int)


-- テトリミノデータ
data TetriMino = TetriMino {
    getMinoBlock    :: Block,
    getMinoPos      :: Pos,
    getMinoShape    :: [Pos]
}deriving Show


-- I型テトリミノ
i_Mino = TetriMino I (5, 2) [(0,0),(0,-1),(0,-2),(0,1)]



replace :: [a] -> a -> Int -> [a]
replace f e p = xs ++ [e] ++ ys
    where   xs = take p f
            ys = tail $ drop p f

-- フィールドの指定座標を指定の要素に置き換える
putBlock :: Field -> Block -> Pos -> Field
putBlock f b p = replace f replaceRow (snd p)
    where   replaceRow = replace (f !! snd p) b (fst p)


-- テトリミノをフィールドに反映
putMino :: TetriMino -> Field -> Field
putMino m f = foldl (\f x -> putBlock f b x) f s
    where   p = getMinoPos m
            s = map (\x -> ((fst x)+(fst p), (snd x)+(snd p))) (getMinoShape m)
            b = getMinoBlock m



