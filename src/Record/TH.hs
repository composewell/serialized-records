{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ExistentialQuantification #-}

module Record.TH where

import Control.Monad (void)
import Data.Int (Int16, Int32)
import Data.Proxy (Proxy(..))
import Data.List (sortBy, findIndex, find, foldl1', foldl', sortOn, elemIndex)
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

getConAndRecFields :: [DataCon] -> Q (Name, [(Name, Type)])
getConAndRecFields cons = do
    let con =
            case cons of
                [x] -> x
                _ -> error "Found more than 1 constructor"
        fields =
            map (\(x, y) -> (fromJustErr "No key found" x, y)) $ dcFields con
    pure $ (dcName con, fields)

getRecFields :: [DataCon] -> Q [(Name, Type)]
getRecFields = fmap snd . getConAndRecFields

repeatedApply :: Int -> Q Exp -> Q Exp -> Q Exp
repeatedApply i _ expr | i <= 0 = expr
repeatedApply i aExpr expr =
    repeatedApply (i - 1) aExpr [|$(aExpr) $(expr)|]

expRecordMeta :: Q [Dec] -> Q Exp
expRecordMeta decs = withReifiedApps decs $ \_ _ _ cons -> do
    keyList <- fmap (zip [0..]) (getRecFields cons)
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

expTypeHash :: RecordStaticMeta -> Q [Dec] -> Q Exp
expTypeHash (RecordStaticMeta rsmList) decs = withReifiedApps decs $ \_ _ _ cons -> do
    keyList <- map (\(a, b) -> (nameBase a, b)) <$> getRecFields cons
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

    respectingNullability re =
        repeatedApply
            (reNullabilityLevel re - 1)
            [|flattenNullable|]
            [|toValue $(varE (prefixName "a_" (reKey re)))|]

    makeLetDec re =
        valD
            (varP (prefixName "v_" (reKey re)))
            (normalB (respectingNullability re))
            []

expSize :: Int -> [RecordElement] -> Q Exp
expSize headerLen rsmList =
    foldl'
        (\b a -> [|recPrimAddSizeTo $(b) $(a)|])
        [|headerLen
              + lenVersion + lenMessageLen + lenTypeHash + lenHeaderLen|]
        (map (varE . prefixName "v_" . reKey) rsmList)

stmtPreHeaderSerialization
    :: GlobalNameSpace -> RecordStaticMeta -> Type -> [Q Stmt]
stmtPreHeaderSerialization ns (RecordStaticMeta rsmList) headTy =
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
             (typeHash (Proxy :: $([t|Proxy $(pure headTy)|])))|]
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
    let nNames = map makeN [0..]
        valNamesAndOffsets = zip rsmList nNames
     in concatMap makeBindS $ zip [iOffset..] valNamesAndOffsets

    where

    makeBindS (i, (val, offName)) =
     let vPrefixedName = (prefixName "v_" (reKey val))
     in if reNullabilityLevel val == 0
        then
            [ noBindS
                  [|void
                     $ recPrimSerializeAt
                           $(varE offName)
                           $(varE arrName)
                           (i_i32 $(varE (makeI i)) :: Int32)|]
            , bindS
                  (varP (makeI (i + 1)))
                  [|recPrimSerializeAt
                     $(varE (makeI i))
                     $(varE arrName)
                     $(varE vPrefixedName)|]
            ]
        else
            [ bindS
                (varP (makeI (i + 1)))
                [|case $(varE vPrefixedName) of
                      Nothing -> do
                          void
                              $ recPrimSerializeAt
                                    $(varE offName)
                                    $(varE arrName)
                                    (0 :: Int32)
                          pure $(varE (makeI i))
                      Just valToSerialize -> do
                          void
                              $ recPrimSerializeAt
                                    $(varE offName)
                                    $(varE arrName)
                                    (i_i32 $(varE (makeI i)) :: Int32)
                          recPrimSerializeAt
                              $(varE (makeI i))
                              $(varE arrName)
                              valToSerialize
                 |]
            ]

    arrName = nsArr ns
    numFields = length rsmList
    iOffset = 4 + numFields

stmtRecordCreation ::
    GlobalNameSpace -> RecordStaticMeta -> Type -> [Q Stmt]
stmtRecordCreation ns rsm@(RecordStaticMeta rsmList) headTy =
    concat
        [ stmtPreHeaderSerialization ns rsm headTy
        , stmtHeaderSerialzation ns rsm
        , stmtBodySerialization ns rsm
        , [noBindS
               [|pure $ Record True $ Array arr 0 $(varE (makeI lastIOffset))|]]
        ]

    where

    numFields = length rsmList
    lastIOffset = numFields * 2 + 4

expCreateRecord :: RecordStaticMeta -> Type -> [DataCon] -> Q Exp
expCreateRecord rsm@(RecordStaticMeta rsmList) headTy cons = do
    let argName = nsArg defaultNameSpace
    (conName, _) <- getConAndRecFields cons
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
        doE $ stmtRecordCreation defaultNameSpace rsm headTy


decIsRecordableInstance ::
    Array Word8 -> RecordStaticMeta -> Type -> [DataCon] -> Q Dec
decIsRecordableInstance typeHashArr rsm@(RecordStaticMeta rsmList) headTy cons =
    instanceD
        (pure [])
        [t|IsRecordable $(pure headTy)|]
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
                    (normalB (expCreateRecord rsm headTy cons))
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

decsIsRecordableInstance ::
    Array Word8 -> RecordStaticMeta -> Type -> [DataCon] -> Q [Dec]
decsIsRecordableInstance a b c d = fmap (:[]) $ decIsRecordableInstance a b c d

expGetFieldTrusted ::
    (Int, Int, Bool) -> RecordElement -> ((Int, Int, Bool), Q Exp)
expGetFieldTrusted (preKeyOffset, preValueOffset, isPrevStatic) re = do
    case reStaticSize re of
        Just valSize ->
            ( (keyPos + 4, preValueOffset + valSize, True)
            , [|getFieldTrustedStatic preValueOffset $(varE arrName)|]
            )
        Nothing ->
            if reNullabilityLevel re == 0
            then if isPrevStatic
                 then
                     ( (keyPos + 4, undefined, False)
                     , [|getFieldTrustedStatic preValueOffset $(varE arrName)|]
                     )
                 else
                     ( (keyPos + 4, undefined, False)
                     , [|getFieldTrustedDynamic keyPos $(varE arrName)|]
                     )
            else
                ( (keyPos + 4, undefined, False)
                , [|case getFieldTrustedNullable keyPos $(varE arrName) of
                        Nothing -> Nothing
                        Just res ->
                            let $(varP (mkName "tmp")) = res
                             in $(repeatedApply
                                    (reNullabilityLevel re)
                                    [|Just|]
                                    (varE (mkName "tmp")))
                  |]
                )

    where

    keyPos = preKeyOffset + length (reKey re) + 2
    arrName = nsArr defaultNameSpace

expGetFieldUntrusted :: Int -> RecordElement -> Q Exp
expGetFieldUntrusted headerLen re =
    if reNullabilityLevel re == 0
    then
        [|fromJust
              $ getFieldUntrusted
                    headerLen
                    (encodeSimpleString $(litE (stringL (reKey re))))
                    $(varE arrName)
        |]
    else
        [|case
             getFieldUntrusted
                 headerLen
                 (encodeSimpleString $(litE (stringL (reKey re))))
                 $(varE arrName) of
              Nothing -> Nothing
              Just res ->
                  let $(varP (mkName "tmp")) = res
                   in $(repeatedApply
                          (reNullabilityLevel re)
                          [|Just|]
                          (varE (mkName "tmp")))
        |]

    where
    arrName = nsArr defaultNameSpace

decHasField ::
    Int -> Type -> (Int, Int, Bool) -> (RecordElement, Type) -> ((Int, Int, Bool), Q Dec)
decHasField headerLen headTy offsets (re, typ) =
    let (offsets1, expGFT) = expGetFieldTrusted offsets re
     in (offsets1, decHF expGFT)

    where

    arrName = nsArr defaultNameSpace
    decHF expGFT =
        instanceD
            (pure [])
            [t|HasField
                  (Proxy $(litT (strTyLit (reKey re))))
                  (Record $(pure headTy))
                  $(pure typ)
            |]
            [ funD
                  'getField
                  [ clause
                        [ wildP
                        , conP 'Record [conP 'True [], varP arrName]
                        ]
                        (normalB expGFT)
                        []
                  , clause
                        [ wildP
                        , conP 'Record [conP 'False [], varP arrName]
                        ]
                        (normalB (expGetFieldUntrusted headerLen re))
                        []
                  ]
            ]

decsHasField :: RecordStaticMeta -> Type -> [DataCon] -> Q [Dec]
decsHasField (RecordStaticMeta rsmList) headTy cons = do
    (conTypeName, conFields) <- getConAndRecFields cons
    let rElements =
            map
                (\x -> ( x
                       , snd
                             $ fromJust
                             $ find
                                   (\(y, _) -> nameBase y == reKey x)
                                   conFields))
                rsmList
    sequence $ go conTypeName initialOffsets rElements []

    where

    headerLen = getHeaderLength rsmList
    initialOffsets = (offsetHeaderBody, offsetMessageBody headerLen, True)
    go _ _ [] ys = ys
    go conTypeName offsets (x:xs) ys =
        let (offsets1, qdec) =
                decHasField headerLen headTy offsets x
         in go conTypeName offsets1 xs (qdec:ys)

deriveSerializedRecInstances ::
    Array Word8 -> RecordStaticMeta -> Q [Dec] -> Q [Dec]
deriveSerializedRecInstances typeHashArr rsm decs = withReifiedApps decs $ \_ _ headTy cons -> do
    recInst <- decIsRecordableInstance typeHashArr rsm headTy cons
    hasFldInsts <- decsHasField rsm headTy cons
    pure $ recInst:hasFldInsts

withReifiedApps ::
    Q [Dec] -> (Maybe Overlap -> Cxt -> Type -> [DataCon] -> Q b) -> Q b
withReifiedApps mDecs next = do
    dec <- mDecs
    case dec of
        [InstanceD mo preds headTyWC []] -> do
            let headTy = unwrap dec headTyWC
                (mainTyName, subs) = getMainTypeName dec headTy
            dt <- reifyDataType mainTyName
            let tyVars = dtTvs dt
                mapper = mapperWith (VarT <$> tyVars) subs
                cons = map (modifyConVariables mapper) (dtCons dt)
            next mo preds headTy cons
        _ -> errorMessage dec

    where

    mapperWith l1 l2 a =
        case elemIndex a l1 of
            Nothing -> a
            -- XXX Capture this case and give a relavant error.
            Just i -> l2 !! i

    mapType f (AppT t1 t2) = AppT (mapType f t1) (mapType f t2)
    mapType f (InfixT t1 n t2) = InfixT (mapType f t1) n (mapType f t2)
    mapType f (UInfixT t1 n t2) = UInfixT (mapType f t1) n (mapType f t2)
    mapType f (ParensT t) = ParensT (mapType f t)
    mapType f v = f v

    modifyConVariables f con =
        con { dcFields = map (\(a, b) -> (a, mapType f b)) (dcFields con) }

    errorMessage dec =
        error $ unlines
            [ "Error: deriveUnbox:"
            , ""
            , ">> " ++ pprint dec
            , ""
            , "The supplied declaration not a valid instance declaration."
            , "Provide a valid Haskell instance declaration without a body."
            , ""
            , "Examples:"
            , "instance Unbox (Proxy a)"
            , "instance Unbox a => Unbox (Identity a)"
            , "instance Unbox (TableT Identity)"
            ]

    unwrap _ (AppT (ConT _) r) = r
    unwrap dec _ = errorMessage dec

    getMainTypeName dec = go []

        where

        go xs (ConT nm) = (nm, xs)
        go xs (AppT l r) = go (r:xs) l
        go _ _ = errorMessage dec
