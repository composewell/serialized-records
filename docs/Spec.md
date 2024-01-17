# Specification

## Goal

1. Replicate most of the features of JSON
2. Be as fast as possible when reading a key
2. Be as fast as possible when creating the record
3. Be user friendly
4. Be less invasive

## Structure

### Message

```
 2 bytes := version :: Int16
 4 bytes := length of message :: Int32
32 bytes := type hash
 2 bytes := length of header :: Int32
 h bytes := header :: [HeaderElement]
 m bytes := body :: [BodyElement]
```

### HeaderElement

```
2 bytes := length of key :: Int16
k bytes := key
4 bytes := index of the value in the body :: Int32
```

### BodyElement

```
1 byte  := boolean that denotes the presense of the field
f bytes := serialized value of the field
```

## Types

The serialized data can understand the following types:

1. strings :: `Array Word8`
2. numbers :: `Int64, Double, Float...`
3. boolean :: `Bool`
4. null    :: `Maybe`
5. records :: `Record`
6. lists   :: `[a]`

# Influence

## Protobuf

TODO

## JSON

TODO

## superrecord

TODO

## large-anon

TODO
