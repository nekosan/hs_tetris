
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


-- テトリミノをフィールドから削除
removeMino :: TetriMino -> Field -> Field
removeMino m f = foldl (\f x -> putBlock f b x) f s
    where   p = getMinoPos m
            s = map (\x -> ((fst x)+(fst p), (snd x)+(snd p))) (getMinoShape m)
            b = E



-- フィールドの指定座標に指定されたブロックが存在するか
checkExistBlock :: Field -> Block -> Pos -> Bool
checkExistBlock f b p = checkBlock == b
    where   checkRow = f !! (snd p)
            checkBlock = checkRow !! (fst p)


-- 指定されたテトリミノがフィールドに存在するかどうか
checkExistMino :: TetriMino -> Field -> Bool
checkExistMino m f = foldl (\result x -> result && (checkExistBlock f b x)) True s
    where   p = getMinoPos m
            s = map (\x -> ((fst x) + (fst p), (snd x) + (snd p))) (getMinoShape m)
            b = getMinoBlock m


-- 指定座標がEmptyかを返す
isEmptyField :: Pos -> Field -> Bool
isEmptyField p f
    | (snd p) < 0 || (snd p) >= (length f) = False
    | (fst p) < 0 || (fst p) >= (length checkRow) = False
    | otherwise = E == checkBlock
    where   checkRow = f !! (snd p)
            checkBlock = checkRow !! (fst p)


-- テトリミノが存在可能かを返す
canExistMino :: TetriMino -> Field -> Bool
canExistMino m f = foldl (\result x -> result && (isEmptyField x f)) True s
    where   p = getMinoPos m
            s = map (\x -> ((fst x) + (fst p), (snd x) + (snd p))) (getMinoShape m)



-- テトリミノを移動する
moveMino :: TetriMino -> Pos -> Field -> TetriMino
moveMino m p f
    | canExistMino newMino f == False = m
    | otherwise = newMino
    where   newMino = TetriMino (getMinoBlock m) p (getMinoShape m)


-- テトリミノの形を回転させる
rotateMino :: TetriMino -> Int-> Field -> TetriMino
rotateMino m d f
    | canExistMino newMino f == False = m
    | otherwise = newMino
    where   rotMat = [(0,1), (1,0), (0,-1), (-1,0)] !! d
            rotation p =    let x = fst p; y = snd p; s = fst rotMat; c = snd rotMat
                            in (x * c + y * s, -x * s + y * c)
            rotatedShape = map rotation (getMinoShape m)
            newMino = TetriMino (getMinoBlock m) (getMinoPos m) rotatedShape


