{
module Tokens where
}

%wrapper "posn"

$alpha = [a-zA-z] -- alphabetic characters
$stringcontents = [a-zA-z0-9\$\-\s$white]

-- tokens additionally store position numbers with the "posn" wrapper
tokens :-
  $white+                         ;                                  -- ignore whitespaces
  exec                            { tok (\p s -> TKExec p) }         -- keyword exec
  split                           { tok (\p s -> TKSplit p) }        -- keyword split
  finally                         { tok (\p s -> TKFinally p) }      -- keyword finally
  \=\=                            { tok (\p s -> TKEquals p)}        -- token ==
  \!\=                            { tok (\p s -> TKNotEquals p)}     -- token !=
  \:                              { tok (\p s -> TKColon p) }        -- token :
  \{                              { tok (\p s -> TKOpenBracket p) }  -- token {
  \}                              { tok (\p s -> TKCloseBracket p) } -- token }
  \-                              { tok (\p s -> TKMinus p) }        -- token -
  \=                              { tok (\p s -> TKAssignment p) }   -- token =
  \;                              { tok (\p s -> TKSemicolon p) }    -- token ;
  $alpha+                         { tok (\p s -> TKName p s)}        -- names
  \"$stringcontents*\"            { tok (\p s -> TKString p s)}      -- strings

-- "
{

tok f p s = f p s

-- definition of Token data type
data Token =
       TKExec AlexPosn          |
       TKSplit AlexPosn         |
       TKFinally AlexPosn       |
       TKEquals AlexPosn        |
       TKNotEquals AlexPosn     |
       TKColon AlexPosn         |
       TKOpenBracket AlexPosn   |
       TKCloseBracket AlexPosn  |
       TKMinus AlexPosn         |
       TKAssignment AlexPosn    |
       TKSemicolon AlexPosn     |
       TKName AlexPosn String   |
       TKString AlexPosn String |
       TKLexError AlexPosn String
       deriving (Eq, Show)

-- returns the position number of each token
token_posn (TKExec p)         = p
token_posn (TKSplit p)        = p
token_posn (TKFinally p)      = p
token_posn (TKEquals p)       = p
token_posn (TKNotEquals p)    = p
token_posn (TKColon p)        = p
token_posn (TKOpenBracket p)  = p
token_posn (TKCloseBracket p) = p
token_posn (TKMinus p)        = p
token_posn (TKAssignment p)   = p
token_posn (TKSemicolon p)    = p
token_posn (TKName p _)       = p
token_posn (TKString p _)     = p
token_posn (TKLexError p _)   = p

-- returns the zero-based row number of the token
tokenGetRow :: Token -> Int
tokenGetRow (TKExec (AlexPn _ r _))         = r-1
tokenGetRow (TKSplit (AlexPn _ r _))        = r-1
tokenGetRow (TKFinally (AlexPn _ r _))      = r-1
tokenGetRow (TKEquals (AlexPn _ r _))       = r-1
tokenGetRow (TKNotEquals (AlexPn _ r _))    = r-1
tokenGetRow (TKColon (AlexPn _ r _))        = r-1
tokenGetRow (TKOpenBracket (AlexPn _ r _))  = r-1
tokenGetRow (TKCloseBracket (AlexPn _ r _)) = r-1
tokenGetRow (TKMinus (AlexPn _ r _))        = r-1
tokenGetRow (TKAssignment (AlexPn _ r _))   = r-1
tokenGetRow (TKSemicolon (AlexPn _ r _))    = r-1
tokenGetRow (TKName (AlexPn _ r _) _)       = r-1
tokenGetRow (TKString (AlexPn _ r _) _)     = r-1
tokenGetRow (TKLexError (AlexPn _ r _) _)   = r-1

-- returns the zero-based column number of the token
tokenGetColumn :: Token -> Int
tokenGetColumn (TKExec (AlexPn _ _ c))         = c-1
tokenGetColumn (TKSplit (AlexPn _ _ c))        = c-1
tokenGetColumn (TKFinally (AlexPn _ _ c))      = c-1
tokenGetColumn (TKEquals (AlexPn _ _ c))       = c-1
tokenGetColumn (TKNotEquals (AlexPn _ _ c))    = c-1
tokenGetColumn (TKColon (AlexPn _ _ c))        = c-1
tokenGetColumn (TKOpenBracket (AlexPn _ _ c))  = c-1
tokenGetColumn (TKCloseBracket (AlexPn _ _ c)) = c-1
tokenGetColumn (TKMinus (AlexPn _ _ c))        = c-1
tokenGetColumn (TKAssignment (AlexPn _ _ c))   = c-1
tokenGetColumn (TKSemicolon (AlexPn _ _ c))    = c-1
tokenGetColumn (TKName (AlexPn _ _ c) _)       = c-1
tokenGetColumn (TKString (AlexPn _ _ c) _)     = c-1
tokenGetColumn (TKLexError (AlexPn _ _ c) _)   = c-1

--- returns a string representation of the token
tokenGetValue :: Token -> String
tokenGetValue (TKExec _)         = "exec"
tokenGetValue (TKSplit _)        = "split"
tokenGetValue (TKFinally _)      = "finally"
tokenGetValue (TKEquals _)       = "=="
tokenGetValue (TKNotEquals _)    = "!="
tokenGetValue (TKColon _)        = ":"
tokenGetValue (TKOpenBracket _)  = "{"
tokenGetValue (TKCloseBracket _) = "}"
tokenGetValue (TKMinus _)        = "-"
tokenGetValue (TKAssignment _)   = "="
tokenGetValue (TKSemicolon _)    = ";"
tokenGetValue (TKName _ v)       = v
tokenGetValue (TKString _ v)     = v
tokenGetValue (TKLexError _ v)   = v

-- returns a "TKLexError" token in case of a lexical error
alexScanErrors str = go (alexStartPos,'\n',[],str)
  where go inp@(pos,_,_,str) =
          case alexScan inp 0 of
                AlexEOF -> []
                AlexError (apn,_,_,str)  -> [TKLexError apn str] --error $ "lexical error at line " ++ (show line) ++ ", column " ++ (show column)
                AlexSkip  inp' len     -> go inp'
                AlexToken inp' len act -> go inp'

{- ORIGINAL by Alex
alexScanTokens str = go (alexStartPos,'\n',[],str)
  where go inp@(pos,_,_,str) =
          case alexScan inp 0 of
                AlexEOF -> []
                AlexError ((AlexPn _ line column),_,_,_) -> error $ "lexical error at line " ++ (show line) ++ ", column " ++ (show column)
                AlexSkip  inp' len     -> go inp'
                AlexToken inp' len act -> act pos (take len str) : go inp'
-}

}