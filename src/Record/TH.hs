{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ExistentialQuantification #-}

module Record.TH where

import Control.Monad (void)
import Data.Int (Int16, Int32)
import Data.Proxy (Proxy(..))
import Data.List (sortBy, findIndex, foldl1', foldl', sortOn)
import Data.Maybe (fromJust)
import Data.Word (Word8)
import Streamly.Internal.Data.Array (Array(..))

import qualified Streamly.Data.Array as Array
import qualified Streamly.Internal.Data.MutByteArray as Serialize

import Record.Types
import Language.Haskell.TH
import Streamly.Internal.Data.MutByteArray

data RecordElement =
    RecordElement
        { reKey :: String
        , reIsStatic :: Bool
        , reNullabilityLevel :: Int
        }
    deriving (Show)

data RecordStaticMeta =
    RecordStaticMeta
        { rsmKeyList :: [RecordElement]
        }
    deriving (Show)

data GlobalNameSpace =
    GlobalNameSpace
        { nsArr :: Name
        , nsArg :: Name
        , nsSize :: Name
        }

defaultNameSpace :: GlobalNameSpace
defaultNameSpace =
    GlobalNameSpace
        { nsArr = mkName "arr"
        , nsArg = mkName "arg"
        , nsSize = mkName "size"
        }

getHeaderLength :: [RecordElement] -> Int
getHeaderLength = sum . map ((+6) . length . reKey)

getConAndRecFields :: Name -> Q (Name, [(Name, Type)])
getConAndRecFields recTypeName = do
    dt <- reifyDataType recTypeName
    let con =
            case dtCons dt of
                [x] -> x
                _ -> error "Found more than 1 constructor"
        fields =
            map (\(x, y) -> (fromJustErr "No key found" x, y)) $ dcFields con
    pure $ (dcName con, fields)

getRecFields :: Name -> Q [(Name, Type)]
getRecFields = fmap snd . getConAndRecFields

createRecordMeta :: Name -> Q Exp
createRecordMeta recTypeName = do
    keyList <- getRecFields recTypeName
    let recElemList = map fieldToRE keyList
    res <- [|RecordStaticMeta (sortRecElemList $(listE recElemList))|]
    pure res

    where

    fieldToRE :: (Name, Type) -> Q Exp
    fieldToRE (x, typ) = do
        let proxyType = [t|Proxy $(pure typ)|]
        let xStr = nameBase x
        res <-
            [|RecordElement xStr
               (recPrimIsStatic (toValueProxy (Proxy :: $(proxyType))))
               (nullabilityLevel (toValueProxy (Proxy :: $(proxyType))))
            |]
        pure res

sortRecElemList :: [RecordElement] -> [RecordElement]
sortRecElemList = sortBy sorterFunc

    where

    sorterFunc (RecordElement _ False _) (RecordElement _ True _) = GT
    sorterFunc (RecordElement _ True _) (RecordElement _ False _) = LT
    sorterFunc (RecordElement a _ _) (RecordElement b _ _) = compare a b

indexList :: Eq c => (a -> c) -> (b -> c) -> [a] -> [b] -> [Int]
indexList mapperA mapperB listA listB =
    map mapper listB

    where

    mapper b = fromJust $ findIndex (\a -> mapperA a == mapperB b) listA

sortUsing :: Eq c => (a -> c) -> (b -> c) -> [a] -> [b] -> [b]
sortUsing mapperA mapperB listA listB =
    sortBy sorter listB

    where

    sorter b1 b2 =
        compare
            (fromJust $ findIndex (\a -> mapperA a == mapperB b1) listA)
            (fromJust $ findIndex (\a -> mapperA a == mapperB b2) listA)

createTypeHashExp :: RecordStaticMeta -> Name -> Q Exp
createTypeHashExp (RecordStaticMeta rsmList) recTypeName = do
    keyList <- map (\(a, b) -> (nameBase a, b)) <$> getRecFields recTypeName
    let sortedKeyList = sortUsing reKey fst rsmList keyList
    foldl1' (\b a -> [|$(b) <> $(a)|]) $ map toHash sortedKeyList

    where

    toHash (n, t) =
        [|encodeSimpleString n
           <> recPrimHash (toValueProxy (Proxy :: Proxy $(pure t)))|]

makeV :: Int -> Name
makeV i = mkName $ "v" ++ show i

makeConvertedValueStatements :: Int -> Q Stmt
makeConvertedValueStatements numFields =
    letS $ map makeLetDec [0..(numFields - 1)]

    where

    makeLetDec i =
        valD (varP (makeV i)) (normalB [|toValue $(varE (mkFieldName i))|]) []

makeSizeExp :: Int -> Int -> Q Exp
makeSizeExp numFields headerLen =
    foldl'
        (\b a -> [|recPrimAddSizeTo $(b) $(a)|])
        [|headerLen
              + lenVersion + lenMessageLen + lenTypeHash + lenHeaderLen|]
        (map (varE . makeV) [0..(numFields - 1)])

createPreHeaderSerializationStatements
    :: GlobalNameSpace -> Array Word8 -> RecordStaticMeta -> [Q Stmt]
createPreHeaderSerializationStatements ns typeHashArr (RecordStaticMeta rsmList) =
    [ makeConvertedValueStatements (length rsmList)
    , letS [valD (varP sizeName) (normalB (makeSizeExp numFields headerLen)) []]
    , bindS (varP arrName) [|Serialize.new size|]
    , letS [valD (varP (makeI 0)) (normalB [|0|]) []]
    , bindS
          (varP (makeI 1))
          [|recPrimSerializeAt $(varE (makeI 0)) $(varE arrName) (0 :: Int16)|]
    , bindS
          (varP (makeI 2))
          [|recPrimSerializeAt
             $(varE (makeI 1)) $(varE arrName) (i_i32 $(varE sizeName))|]
    , bindS
          (varP (makeI 3))
          [|unsafePutCompleteSlice
             $(varE (makeI 2)) $(varE arrName)
             (Array.fromList (take 32 ($(arrExp) :: [Word8])))|]
    , bindS
          (varP (makeI 4))
          [|recPrimSerializeAt
             $(varE (makeI 3)) $(varE arrName) (headerLen :: Int16)|]
    ]

    where

    sizeName = nsSize ns
    arrName = nsArr ns
    arrExp =
        listE $ map (litE . integerL . fromIntegral) $ Array.toList typeHashArr
    numFields = length rsmList
    headerLen = getHeaderLength rsmList

createHeaderSerialzationStatements :: GlobalNameSpace -> RecordStaticMeta -> [Q Stmt]
createHeaderSerialzationStatements ns (RecordStaticMeta rsmList) =
    map
        (\(i, re) -> makeBindS i (reKey re))
        (zip [iOffest..(iOffest + (length rsmList - 1))] rsmList)

    where

    iOffest = 4

    arrName = nsArr ns

    makeBindS i keyStr =
        bindS
            (tupP [varP (makeN (i - iOffest)), varP (makeI (i + 1))])
            [|unsafePutHeaderKey
               $(varE (makeI i))
               $(varE arrName)
               $(stringE keyStr)
            |]

createBodySerializationStatements
    :: GlobalNameSpace -> [(Name, Type)] -> RecordStaticMeta -> [Q Stmt]
createBodySerializationStatements ns conFields (RecordStaticMeta rsmList) =
    let indexedFields = indexList reKey (nameBase . fst) rsmList conFields
        valueNames = map (makeV . fst) $ sortOn snd $ (zip [0..] indexedFields)
        nNames = map makeN [0..]
        valNamesAndOffsets = zip valueNames nNames
     in concatMap makeBindS $ zip [iOffset..] valNamesAndOffsets

    where

    makeBindS (i, (valName, offName)) =
        [ noBindS
              [|void
                 $ recPrimSerializeAt
                       $(varE offName)
                       $(varE arrName)
                       (i_i32 $(varE (makeI i)) :: Int32)|]
        , bindS
              (varP (makeI (i + 1)))
              [|recPrimSerializeAt
                 $(varE (makeI i)) $(varE arrName) $(varE valName)|]
        ]

    arrName = nsArr ns
    numFields = length rsmList
    iOffset = 4 + numFields

createStatements
    :: GlobalNameSpace -> [(Name, Type)] -> Array Word8 -> RecordStaticMeta -> [Q Stmt]
createStatements ns reified typeHashArr rsm@(RecordStaticMeta rsmList) =
    concat
        [ createPreHeaderSerializationStatements ns typeHashArr rsm
        , createHeaderSerialzationStatements ns rsm
        , createBodySerializationStatements ns reified rsm
        , [noBindS
               [|pure $ Record True $ Array arr 0 $(varE (makeI lastIOffset))|]]
        ]

    where

    numFields = length rsmList
    lastIOffset = numFields * 2 + 4

createCreateRecordExp :: Array Word8 -> RecordStaticMeta -> Name -> Q Exp
createCreateRecordExp typeHashArr rsm recTypeName = do
    let argName = nsArg defaultNameSpace
    (conName, conFields) <- getConAndRecFields recTypeName
    let numFields = length conFields
    caseE
        (varE argName)
        [matchConstructor
             conName
             numFields
             (doE (createStatements defaultNameSpace conFields typeHashArr rsm))]
