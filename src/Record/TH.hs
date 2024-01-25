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
import System.IO.Unsafe (unsafePerformIO)

import qualified Streamly.Data.Array as Array
import qualified Streamly.Internal.Data.MutByteArray as Serialize

import Record.Types
import Language.Haskell.TH
import Streamly.Internal.Data.MutByteArray

data RecordElement =
    RecordElement
        { reKey :: String
        , reOrigIndex :: Int
        , reStaticSize :: Maybe Int
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

expRecordMeta :: Name -> Q Exp
expRecordMeta recTypeName = do
    keyList <- fmap (zip [0..]) (getRecFields recTypeName)
    let recElemList = map fieldToRE keyList
    res <- [|RecordStaticMeta (sortRecElemList $(listE recElemList))|]
    pure res

    where

    fieldToRE :: (Int, (Name, Type)) -> Q Exp
    fieldToRE (i, (x, typ)) = do
        let proxyType = [t|Proxy $(pure typ)|]
        let xStr = nameBase x
        res <-
            [|RecordElement xStr i
               (recPrimStaticSize (toValueProxy (Proxy :: $(proxyType))))
               (nullabilityLevel (toValueProxy (Proxy :: $(proxyType))))
            |]
        pure res

sortRecElemList :: [RecordElement] -> [RecordElement]
sortRecElemList = sortBy sorterFunc

    where

    sorterFunc (RecordElement _ _ Nothing _) (RecordElement _ _ (Just _) _) = GT
    sorterFunc (RecordElement _ _ (Just _) _) (RecordElement _ _ Nothing _) = LT
    sorterFunc (RecordElement a _ _ _) (RecordElement b _ _ _) = compare a b

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

expTypeHash :: RecordStaticMeta -> Name -> Q Exp
expTypeHash (RecordStaticMeta rsmList) recTypeName = do
    keyList <- map (\(a, b) -> (nameBase a, b)) <$> getRecFields recTypeName
    let sortedKeyList = sortUsing reKey fst rsmList keyList
    foldl1' (\b a -> [|$(b) <> $(a)|]) $ map toHash sortedKeyList

    where

    toHash (n, t) =
        [|encodeSimpleString n
           <> recPrimHash (toValueProxy (Proxy :: Proxy $(pure t)))|]

prefixName :: String -> String -> Name
prefixName p v = mkName $ p ++ v

stmtToValue :: [RecordElement] -> Q Stmt
stmtToValue rsmList =
    letS $ map makeLetDec rsmList

    where

    makeLetDec re =
        valD
            (varP (prefixName "v_" (reKey re)))
            (normalB [|toValue $(varE (prefixName "a_" (reKey re)))|]) []

expSize :: Int -> [RecordElement] -> Q Exp
expSize headerLen rsmList =
    foldl'
        (\b a -> [|recPrimAddSizeTo $(b) $(a)|])
        [|headerLen
              + lenVersion + lenMessageLen + lenTypeHash + lenHeaderLen|]
        (map (varE . prefixName "v_" . reKey) rsmList)

stmtPreHeaderSerialization
    :: GlobalNameSpace -> RecordStaticMeta -> Name -> [Q Stmt]
stmtPreHeaderSerialization ns (RecordStaticMeta rsmList) recTypeName =
    [ stmtToValue rsmList
    , letS [valD (varP sizeName) (normalB (expSize headerLen rsmList)) []]
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
             (typeHash (Proxy :: $([t|Proxy $(conT recTypeName)|])))|]
    , bindS
          (varP (makeI 4))
          [|recPrimSerializeAt
             $(varE (makeI 3)) $(varE arrName) (headerLen :: Int16)|]
    ]

    where

    sizeName = nsSize ns
    arrName = nsArr ns
    headerLen = getHeaderLength rsmList

stmtHeaderSerialzation :: GlobalNameSpace -> RecordStaticMeta -> [Q Stmt]
stmtHeaderSerialzation ns (RecordStaticMeta rsmList) =
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

stmtBodySerialization :: GlobalNameSpace -> RecordStaticMeta -> [Q Stmt]
stmtBodySerialization ns (RecordStaticMeta rsmList) =
    let valueNames = map (prefixName "v_" . reKey) rsmList
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

stmtRecordCreation :: GlobalNameSpace -> RecordStaticMeta -> Name -> [Q Stmt]
stmtRecordCreation ns rsm@(RecordStaticMeta rsmList) recTypeName =
    concat
        [ stmtPreHeaderSerialization ns rsm recTypeName
        , stmtHeaderSerialzation ns rsm
        , stmtBodySerialization ns rsm
        , [noBindS
               [|pure $ Record True $ Array arr 0 $(varE (makeI lastIOffset))|]]
        ]

    where

    numFields = length rsmList
    lastIOffset = numFields * 2 + 4

expCreateRecord :: RecordStaticMeta -> Name -> Q Exp
expCreateRecord rsm@(RecordStaticMeta rsmList) recTypeName = do
    let argName = nsArg defaultNameSpace
    (conName, _) <- getConAndRecFields recTypeName
    caseE
        (varE argName)
        [match
             (conP conName
                  (map (varP . prefixName "a_" . reKey)
                  (sortOn reOrigIndex rsmList)))
             (normalB [|unsafePerformIO $(expDoRecordCreation)|])
             []
        ]

    where

    expDoRecordCreation =
        doE $ stmtRecordCreation defaultNameSpace rsm recTypeName


decIsRecordableInstance :: Array Word8 -> RecordStaticMeta -> Name -> Q Dec
decIsRecordableInstance typeHashArr rsm@(RecordStaticMeta rsmList) recTypeName =
    instanceD
        (pure [])
        [t|IsRecordable $(conT recTypeName)|]
        [ funD
              'typeHash
              [ clause
                    [wildP]
                    (normalB [|Array.fromList ($(expH) :: [Word8])|])
                    []
              ]
        , funD
              'recStaticSize
              [ clause
                    [wildP]
                    (normalB [|staticSize|])
                    []
              ]
        , funD
              'createRecord
              [ clause
                    [varP (nsArg defaultNameSpace)]
                    (normalB (expCreateRecord rsm recTypeName))
                    []
              ]
        ]

    where

    headerLen = getHeaderLength rsmList
    mBodyLen = fmap sum $ sequence $ map reStaticSize rsmList
    constSizeOverhead = offsetMessageBody headerLen
    staticSize = fmap (+constSizeOverhead) mBodyLen
    expH =
        listE
            $ map (litE . integerL . fromIntegral)
            $ take 32 (Array.toList typeHashArr ++ repeat 0)

decsIsRecordableInstance  :: Array Word8 -> RecordStaticMeta -> Name -> Q [Dec]
decsIsRecordableInstance a b c = fmap (:[]) $ decIsRecordableInstance a b c
