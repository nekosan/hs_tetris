
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
    getMinoBlock        :: Block,
    getMinoPos          :: Pos,
    getMinoShape        :: [Pos],
    getMinoRotate       :: Int,
    getMinoRotateMax    :: Int
}deriving Show


-- テトリミノの移動方向種別
data Move = NONE | LEFT | RIGHT | UP | DOWN deriving (Show, Eq)


-- I型テトリミノ
i_Mino = TetriMino I (5, 2) [(0,0),(0,-1),(0,-2),(0,1)] 0 2



replace :: [a] -> a -> Int -> [a]
replace f e p = xs ++ [e] ++ ys
    where   xs = take p f
            ys = tail $ drop p f

-- フィールドの指定座標を指定の要素に置き換える
putBlock :: Field -> Block -> Pos -> Field
putBlock f b (x,y) = replace f replaceRow y
    where   replaceRow = replace (f !! y) b x


-- テトリミノをフィールドに反映
putMino :: TetriMino -> Field -> Field
putMino m f = foldl (\f x -> putBlock f b x) f s
    where   (fx, fy) = getMinoPos m
            s = map (\(mx, my) -> (mx + fx, my + fy)) (getMinoShape m)
            b = getMinoBlock m


-- テトリミノをフィールドから削除
removeMino :: TetriMino -> Field -> Field
removeMino m f = foldl (\f x -> putBlock f b x) f s
    where   (fx, fy) = getMinoPos m
            s = map (\(mx, my) -> (mx + fx, my + fy)) (getMinoShape m)
            b = E



-- フィールドの指定座標に指定されたブロックが存在するか
checkExistBlock :: Field -> Block -> Pos -> Bool
checkExistBlock f b (x,y) = checkBlock == b
    where   checkRow = f !! y
            checkBlock = checkRow !! x


-- 指定されたテトリミノがフィールドに存在するかどうか
checkExistMino :: TetriMino -> Field -> Bool
checkExistMino m f = foldl (\result x -> result && (checkExistBlock f b x)) True s
    where   (fx, fy) = getMinoPos m
            s = map (\(mx, my) -> (mx + fx, my + fy)) (getMinoShape m)
            b = getMinoBlock m


-- 指定座標がEmptyかを返す
isEmptyField :: Pos -> Field -> Bool
isEmptyField (x,y) f
    | y < 0 || y >= (length f) = False
    | x < 0 || x >= (length checkRow) = False
    | otherwise = E == checkBlock
    where   checkRow = f !! y
            checkBlock = checkRow !! x


-- テトリミノが存在可能かを返す
canExistMino :: TetriMino -> Field -> Bool
canExistMino m f = foldl (\result x -> result && (isEmptyField x f)) True s
    where   (fx, fy) = getMinoPos m
            s = map (\(mx, my) -> (mx + fx, my + fy)) (getMinoShape m)



-- テトリミノを移動する
moveMino :: TetriMino -> Move -> Field -> TetriMino
moveMino m p f
    | canExistMino newMino f == False = m
    | otherwise = newMino
    where   (x, y) = getMinoPos m
            newMino = case p of
                LEFT -> m {getMinoPos = (x-1, y)}
                RIGHT-> m {getMinoPos = (x+1, y)}
                UP   -> m {getMinoPos = (x, y-1)}
                DOWN -> m {getMinoPos = (x, y+1)}
                NONE -> m {getMinoPos = (x, y)}


-- テトリミノを回転させる
rotateMino :: TetriMino -> Field -> TetriMino
rotateMino m f
    | canExistMino newMino f == False = m
    | otherwise = newMino
    where   rot = getMinoRotate m
            rotMax = getMinoRotateMax m
            rotDig = if rot + 1 == rotMax
                        then 4 - rot
                        else 1
            newRot = if rot + 1 == rotMax then 0 else rot + 1
            rotMat = [(0,1), (1,0), (0,-1), (-1,0), (0,1)] !! rotDig
            rotation p =    let (x,y) = p; (s,c) = rotMat
                            in (x * c + y * s, -x * s + y * c)
            rotatedShape = map rotation (getMinoShape m)
            newMino = m {getMinoShape = rotatedShape, getMinoRotate = newRot}




