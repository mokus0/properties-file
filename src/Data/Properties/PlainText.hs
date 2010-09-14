{-# LANGUAGE ViewPatterns #-}
module Data.Properties.PlainText (load, store) where

import Data.Properties.Type

import Data.Char
import qualified Data.Map as M
import Numeric (readHex)
import Text.PrettyPrint

load :: String -> Properties
load = Properties Nothing 
     . M.fromList
     . map (readProperty . splicePropertyLines) 
     . propertyLines 
     . logicalLines 
     . naturalLines

store :: Maybe String -> Properties -> String
store c = unNaturalLines
        . unLogicalLines
        . unPropertyLines c
        . map (uncurry layoutProperty . writeProperty)
        . M.toList
        . propLocals


naturalLines = lines
unNaturalLines = unlines

data LogicalLine a
    = CommentLine String 
    | PropertyLine a
    deriving (Eq, Show)

instance Functor LogicalLine where
    fmap f (CommentLine  c) = CommentLine  c
    fmap f (PropertyLine p) = PropertyLine (f p)

logicalLines [] = []
logicalLines (l:ls)
    | isBlankLine l         = logicalLines ls
    | isCommentLine l       = CommentLine l : logicalLines ls
    | otherwise             = case readContinuingLines (l:ls) of
        ~(l, ls) -> PropertyLine l : logicalLines ls

unLogicalLines [] = []
unLogicalLines (CommentLine l : rest)
    | isCommentLine l || isBlankLine l
                        = l           : unLogicalLines rest
    | otherwise         = ("# " ++ l) : unLogicalLines rest
unLogicalLines (PropertyLine l : rest)
    = writeContinuingLines l ++ unLogicalLines rest

propertyLines ls = [ p | PropertyLine p <- ls]
unPropertyLines Nothing   ls = map PropertyLine ls
unPropertyLines (Just c) ls = map CommentLine (lines c) ++ unPropertyLines Nothing ls

isBlankLine = all isSpace
isCommentLine l = take 1 (dropWhile isSpace l) `elem` ["#", "!"]

asContinuingLine ""   = Nothing
asContinuingLine "\\" = Just ""
asContinuingLine (c:cs) = fmap (c:) (asContinuingLine cs)

readContinuingLines = go id
    where
        go out [] = error "Unexpected EOF when reading continuation line"
        go out (l:ls) = case asContinuingLine l of
            Just l  -> go (out . (l:)) ls
            Nothing -> (out [l], ls)

writeContinuingLines [] = []
writeContinuingLines (reverse -> (l:ls)) = reverse (l : map (++ "\\") ls)

splicePropertyLines [] = []
splicePropertyLines (l:ls) = l ++ concatMap (dropWhile isSpace) ls

readProperty (dropWhile isSpace -> l) = (key, unEscape val)
    where
        (key, dropWhile isSpace -> afterKey) = splitKey l
        val = case afterKey of
            "" -> ""
            (':':rest)              -> dropWhile isSpace rest
            ('=':rest)              -> dropWhile isSpace rest
            other                   -> other

writeProperty (k,v) = (escape k, map escapeLeadingSpace (wrapValue 60 72 v))

layoutProperty key value = lines (show doc)
    where
        doc = hang (text key) 4 (equals <+> cat (map text value))

escape [] = []
escape ('\t':rest) = "\\t"  ++ escape rest
escape ('\n':rest) = "\\n"  ++ escape rest
escape ('\f':rest) = "\\f"  ++ escape rest
escape ('\r':rest) = "\\r"  ++ escape rest
escape ('"' :rest) = "\\\"" ++ escape rest
escape ('\'':rest) = "\\'"  ++ escape rest
escape (':' :rest) = "\\:"  ++ escape rest
escape ('=' :rest) = "\\="  ++ escape rest
escape (' ' :rest) = "\\ "  ++ escape rest
escape (c:rest) = c : escape rest

escapeLeadingSpace str@(c:_)
    | isSpace c = '\\' : str
escapeLeadingSpace str = str

wrapValue minWidth maxWidth val = case splitAt maxWidth val of
    (fits, []) -> [fits]
    (fits, rest) -> case splitAt minWidth fits of
        (minPart, extra) -> case break isSpace (reverse extra) of
            (_, "")
                -> fits : wrapValue minWidth maxWidth rest 
            (reverse -> afterSpace, reverse -> beforeSpace)
                -> (minPart ++ beforeSpace) : wrapValue minWidth maxWidth (afterSpace ++ rest)

unCons ('\\':'u':rest) = case rest of
    a:b:c:d:cs  | all isHexDigit [a,b,c,d]  
        -> (chr . fst . head $ readHex [a,b,c,d], cs)
    other   -> error ("malformed unicode escape: \"\\u" ++ take 4 other ++ "\"")
unCons ('\\':'t' :cs) = ('\t', cs)
unCons ('\\':'n' :cs) = ('\n', cs)
unCons ('\\':'f' :cs) = ('\f', cs)
unCons ('\\':'r' :cs) = ('\r', cs)
unCons ('\\':'"' :cs) = ('"' , cs)
unCons ('\\':'\'':cs) = ('\'', cs)
unCons ('\\':' ' :cs) = (' ' , cs)
unCons (c:cs)         = (c, cs)

unEscape [] = []
unEscape (unCons -> ~(c, cs)) = c : unEscape cs

splitKey = go id
    where
        go out (c:cs)
            | isSpace c || elem c ":="  = (out [], c:cs)
        go out (unCons -> (c, cs))      = go (out . (c:)) cs
