module MD5Fast exposing
    ( hex
    , words, wordsToHex
    )

{-| This library allows you to compute MD5 message digests in Elm. It exposes two
functions that take any string and outputs a fingerprint or "hash" that is always
128 bits long regardless of the size of the input string. The hash can be reported
as either a string of 32 hexadecimal characters or a tuple of 4 integers.
More information about the MD5 algorithm can be found [here](https://en.wikipedia.org/wiki/MD5).

This library is is implemented using the approach for fast JavaScript MD5 hashing
from [Joseph Myers]<http://www.myersdaily.org/joseph/javascript/md5-text.html>
([JavaSctipt source code]<https://gist.github.com/jhoff/7680711>). The API and documentation
matches that of [sanichi/elm-md5]<http://package.elm-lang.org/packages/sanichi/elm-md5/latest>
as closely as possible with the addtion of a function to return the hash as a
4 integer tuple.


# Digest Functions

@docs hex

-}

import Array
import Bitwise exposing (and, complement, or, shiftLeftBy, shiftRightBy, shiftRightZfBy)
import Char


type Int128
    = Words Int Int Int Int


{-| Given a string of arbitrary length, returns a string of 32 hexadecimal characters (a-f, 0-9)
representing the 128-bit MD5 message digest.
hex "" == "d41d8cd98f00b204e9800998ecf8427e"
hex "foobarbaz" == "6df23dc03f9b54cc38a0fc1483df6e21"
Like [sanichi/elm-md5]<http://package.elm-lang.org/packages/sanichi/elm-md5/latest>,
CRLF pairs in the input are not automatically replaced with LFs prior to computing
the digest. If you want that behaviour you should adjust the input yourself before
evaluating the function.
For example:
myHex : String -> String
myHex input =
let
myInput =
Regex.replace Regex.All (Regex.regex "\\x0D\\n") (\_ -> "\\n") input
in
hex myInput
-}
hex : String -> String
hex s =
    words s |> wordsToHex


{-| Given a string of arbitrary length, returns a set of four integers representing
the 128-bit MD5 message digest.
words "" == (-645128748, 78774415, -1744207639, 2118318316)
words "foobarbaz" == "(-1069682067, -866870465, 352100408, 560914307)"
Like [sanichi/elm-md5]<http://package.elm-lang.org/packages/sanichi/elm-md5/latest>,
CRLF pairs in the input are not automatically replaced with LFs prior to computing
the digest.

This integer representation is primarily for applications that wish to
compute the MD5 hash as quickly as possible without the need to convert the hash
into a hex string. A typical use case might be to cycle though a set of input
values until a hash with certain characterstics (e.g. leading zeros) is found.

-}
words : String -> Int128
words s0 =
    let
        state =
            stackLong (Words 1732584193 -271733879 -1732584194 271733878) s0

        len0 =
            String.length s0

        s1 =
            String.dropLeft ((len0 // 64) * 64) s0

        len1 =
            String.length s1

        tail1 =
            newTail [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ] s1

        tail2 =
            nt2 len1 tail1 0x80

        state2 =
            if len1 > 55 then
                md5cycle state tail2

            else
                state

        tail3 =
            if len1 > 55 then
                [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]

            else
                tail2
    in
    md5cycle state2 (set 14 (len0 * 8) tail3)


{-| Converts the given set of 4 integers into a single Hex string.
wordsToHex (0,0,0,0) == "00000000000000000000000000000000"
wordsToHex (0,255,0,0) == 00000000ff0000000000000000000000"
wordsToHex (512,1024,2048,4096) == "00020000000400000008000000100000"
-}
wordsToHex : Int128 -> String
wordsToHex ws =
    let
        rhex n =
            [ hex_chr (and (shiftRightBy 4 n) 0x0F)
            , hex_chr (and n 0x0F)
            , hex_chr (and (shiftRightBy 12 n) 0x0F)
            , hex_chr (and (shiftRightBy 8 n) 0x0F)
            , hex_chr (and (shiftRightBy 20 n) 0x0F)
            , hex_chr (and (shiftRightBy 16 n) 0x0F)
            , hex_chr (and (shiftRightBy 28 n) 0x0F)
            , hex_chr (and (shiftRightBy 24 n) 0x0F)
            ]
    in
    case ws of
        Words a b c d ->
            rhex a ++ rhex b ++ rhex c ++ rhex d |> String.fromList


stackLong : Int128 -> String -> Int128
stackLong state s =
    if String.length s <= 64 then
        state

    else
        stackLong (String.left 64 s |> md5blk |> md5cycle state) (String.dropLeft 64 s)


newTail : List Int -> String -> List Int
newTail tail s =
    let
        cCodes =
            String.toList s |> List.map Char.toCode |> List.indexedMap (\a b -> ( a, b ))
    in
    List.foldl (\( i, c ) -> \t -> nt2 i t c) tail cCodes


nt2 : Int -> List Int -> Int -> List Int
nt2 i tail n =
    case List.drop (i // 4) tail of
        x :: xs ->
            List.take (i // 4) tail ++ or x (shiftLeftBy (shiftLeftBy 3 (modBy 4 i)) n) :: xs

        _ ->
            tail


md5cycle : Int128 -> List Int -> Int128
md5cycle w k =
    case w of
        Words a0 b0 c0 d0 ->
            let
                -- The last integer in each of the following expressions is a direct lookups
                -- from the sine table T[i = 0..63] = int(4294967296 * abs(sin(i+1)))
                a1 =
                    ff a0 b0 c0 d0 (get 0 k) 7 -680876936

                d1 =
                    ff d0 a1 b0 c0 (get 1 k) 12 -389564586

                c1 =
                    ff c0 d1 a1 b0 (get 2 k) 17 606105819

                b1 =
                    ff b0 c1 d1 a1 (get 3 k) 22 -1044525330

                a2 =
                    ff a1 b1 c1 d1 (get 4 k) 7 -176418897

                d2 =
                    ff d1 a2 b1 c1 (get 5 k) 12 1200080426

                c2 =
                    ff c1 d2 a2 b1 (get 6 k) 17 -1473231341

                b2 =
                    ff b1 c2 d2 a2 (get 7 k) 22 -45705983

                a3 =
                    ff a2 b2 c2 d2 (get 8 k) 7 1770035416

                d3 =
                    ff d2 a3 b2 c2 (get 9 k) 12 -1958414417

                c3 =
                    ff c2 d3 a3 b2 (get 10 k) 17 -42063

                b3 =
                    ff b2 c3 d3 a3 (get 11 k) 22 -1990404162

                a4 =
                    ff a3 b3 c3 d3 (get 12 k) 7 1804603682

                d4 =
                    ff d3 a4 b3 c3 (get 13 k) 12 -40341101

                c4 =
                    ff c3 d4 a4 b3 (get 14 k) 17 -1502002290

                b4 =
                    ff b3 c4 d4 a4 (get 15 k) 22 1236535329

                a5 =
                    gg a4 b4 c4 d4 (get 1 k) 5 -165796510

                d5 =
                    gg d4 a5 b4 c4 (get 6 k) 9 -1069501632

                c5 =
                    gg c4 d5 a5 b4 (get 11 k) 14 643717713

                b5 =
                    gg b4 c5 d5 a5 (get 0 k) 20 -373897302

                a6 =
                    gg a5 b5 c5 d5 (get 5 k) 5 -701558691

                d6 =
                    gg d5 a6 b5 c5 (get 10 k) 9 38016083

                c6 =
                    gg c5 d6 a6 b5 (get 15 k) 14 -660478335

                b6 =
                    gg b5 c6 d6 a6 (get 4 k) 20 -405537848

                a7 =
                    gg a6 b6 c6 d6 (get 9 k) 5 568446438

                d7 =
                    gg d6 a7 b6 c6 (get 14 k) 9 -1019803690

                c7 =
                    gg c6 d7 a7 b6 (get 3 k) 14 -187363961

                b7 =
                    gg b6 c7 d7 a7 (get 8 k) 20 1163531501

                a8 =
                    gg a7 b7 c7 d7 (get 13 k) 5 -1444681467

                d8 =
                    gg d7 a8 b7 c7 (get 2 k) 9 -51403784

                c8 =
                    gg c7 d8 a8 b7 (get 7 k) 14 1735328473

                b8 =
                    gg b7 c8 d8 a8 (get 12 k) 20 -1926607734

                a9 =
                    hh a8 b8 c8 d8 (get 5 k) 4 -378558

                d9 =
                    hh d8 a9 b8 c8 (get 8 k) 11 -2022574463

                c9 =
                    hh c8 d9 a9 b8 (get 11 k) 16 1839030562

                b9 =
                    hh b8 c9 d9 a9 (get 14 k) 23 -35309556

                a10 =
                    hh a9 b9 c9 d9 (get 1 k) 4 -1530992060

                d10 =
                    hh d9 a10 b9 c9 (get 4 k) 11 1272893353

                c10 =
                    hh c9 d10 a10 b9 (get 7 k) 16 -155497632

                b10 =
                    hh b9 c10 d10 a10 (get 10 k) 23 -1094730640

                a11 =
                    hh a10 b10 c10 d10 (get 13 k) 4 681279174

                d11 =
                    hh d10 a11 b10 c10 (get 0 k) 11 -358537222

                c11 =
                    hh c10 d11 a11 b10 (get 3 k) 16 -722521979

                b11 =
                    hh b10 c11 d11 a11 (get 6 k) 23 76029189

                a12 =
                    hh a11 b11 c11 d11 (get 9 k) 4 -640364487

                d12 =
                    hh d11 a12 b11 c11 (get 12 k) 11 -421815835

                c12 =
                    hh c11 d12 a12 b11 (get 15 k) 16 530742520

                b12 =
                    hh b11 c12 d12 a12 (get 2 k) 23 -995338651

                a13 =
                    ii a12 b12 c12 d12 (get 0 k) 6 -198630844

                d13 =
                    ii d12 a13 b12 c12 (get 7 k) 10 1126891415

                c13 =
                    ii c12 d13 a13 b12 (get 14 k) 15 -1416354905

                b13 =
                    ii b12 c13 d13 a13 (get 5 k) 21 -57434055

                a14 =
                    ii a13 b13 c13 d13 (get 12 k) 6 1700485571

                d14 =
                    ii d13 a14 b13 c13 (get 3 k) 10 -1894986606

                c14 =
                    ii c13 d14 a14 b13 (get 10 k) 15 -1051523

                b14 =
                    ii b13 c14 d14 a14 (get 1 k) 21 -2054922799

                a15 =
                    ii a14 b14 c14 d14 (get 8 k) 6 1873313359

                d15 =
                    ii d14 a15 b14 c14 (get 15 k) 10 -30611744

                c15 =
                    ii c14 d15 a15 b14 (get 6 k) 15 -1560198380

                b15 =
                    ii b14 c15 d15 a15 (get 13 k) 21 1309151649

                a16 =
                    ii a15 b15 c15 d15 (get 4 k) 6 -145523070

                d16 =
                    ii d15 a16 b15 c15 (get 11 k) 10 -1120210379

                c16 =
                    ii c15 d16 a16 b15 (get 2 k) 15 718787259

                b16 =
                    ii b15 c16 d16 a16 (get 9 k) 21 -343485551
            in
            Words (add32 a16 a0) (add32 b16 b0) (add32 c16 c0) (add32 d16 d0)


cmn : Int -> Int -> Int -> Int -> Int -> Int -> Int
cmn q a b x s t =
    let
        a_ =
            add32 (add32 a q) (add32 x t)
    in
    add32 (or (shiftLeftBy s a_) (shiftRightZfBy (32 - s) a_)) b


ff : Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
ff a b c d x s t =
    cmn (or (and b c) (and (complement b) d)) a b x s t


gg : Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
gg a b c d x s t =
    cmn (or (and b d) (and c (complement d))) a b x s t


hh : Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
hh a b c d x s t =
    cmn (Bitwise.xor (Bitwise.xor b c) d) a b x s t


ii : Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
ii a b c d x s t =
    cmn (Bitwise.xor c (or b (complement d))) a b x s t


md5blk : String -> List Int
md5blk s =
    let
        c i =
            s |> String.toList |> List.map Char.toCode |> Array.fromList |> Array.get i |> Maybe.withDefault 0
    in
    [ c 0 + shiftLeftBy 8 (c 1) + shiftLeftBy 16 (c 2) + shiftLeftBy 24 (c 3)
    , c 4 + shiftLeftBy 8 (c 5) + shiftLeftBy 16 (c 6) + shiftLeftBy 24 (c 7)
    , c 8 + shiftLeftBy 8 (c 9) + shiftLeftBy 16 (c 10) + shiftLeftBy 24 (c 11)
    , c 12 + shiftLeftBy 8 (c 13) + shiftLeftBy 16 (c 14) + shiftLeftBy 24 (c 15)
    , c 16 + shiftLeftBy 8 (c 17) + shiftLeftBy 16 (c 18) + shiftLeftBy 24 (c 19)
    , c 20 + shiftLeftBy 8 (c 21) + shiftLeftBy 16 (c 22) + shiftLeftBy 24 (c 23)
    , c 24 + shiftLeftBy 8 (c 25) + shiftLeftBy 16 (c 26) + shiftLeftBy 24 (c 27)
    , c 28 + shiftLeftBy 8 (c 29) + shiftLeftBy 16 (c 30) + shiftLeftBy 24 (c 31)
    , c 32 + shiftLeftBy 8 (c 33) + shiftLeftBy 16 (c 34) + shiftLeftBy 24 (c 35)
    , c 36 + shiftLeftBy 8 (c 37) + shiftLeftBy 16 (c 38) + shiftLeftBy 24 (c 39)
    , c 40 + shiftLeftBy 8 (c 41) + shiftLeftBy 16 (c 42) + shiftLeftBy 24 (c 43)
    , c 44 + shiftLeftBy 8 (c 45) + shiftLeftBy 16 (c 46) + shiftLeftBy 24 (c 47)
    , c 48 + shiftLeftBy 8 (c 49) + shiftLeftBy 16 (c 50) + shiftLeftBy 24 (c 51)
    , c 52 + shiftLeftBy 8 (c 53) + shiftLeftBy 16 (c 54) + shiftLeftBy 24 (c 55)
    , c 56 + shiftLeftBy 8 (c 57) + shiftLeftBy 16 (c 58) + shiftLeftBy 24 (c 59)
    , c 60 + shiftLeftBy 8 (c 61) + shiftLeftBy 16 (c 62) + shiftLeftBy 24 (c 63)
    ]


hex_chr : Int -> Char
hex_chr n =
    -- This is much quicker than Array.get n (Array.fromList <| String.toList "0123456789abcdef") |> Maybe.withDefault 'f'
    case n of
        0 ->
            '0'

        1 ->
            '1'

        2 ->
            '2'

        3 ->
            '3'

        4 ->
            '4'

        5 ->
            '5'

        6 ->
            '6'

        7 ->
            '7'

        8 ->
            '8'

        9 ->
            '9'

        10 ->
            'a'

        11 ->
            'b'

        12 ->
            'c'

        13 ->
            'd'

        14 ->
            'e'

        _ ->
            'f'


get : Int -> List Int -> Int
get i list =
    -- TODO: Is this as fast as pattern matching against a list or large tuple?
    -- Note this invisibly defaults to 0 if i is out of range
    List.drop i list |> List.head |> Maybe.withDefault 0


set : Int -> Int -> List Int -> List Int
set i n list =
    -- Note this will add a value at the beginning or end of the list if index is out of range
    List.take i list ++ (n :: List.drop (i + 1) list)


add32 : Int -> Int -> Int
add32 a b =
    and (a + b) 0xFFFFFFFF
