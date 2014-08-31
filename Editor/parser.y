{
module Parser where

import Tokens
}

%name        parse
%tokentype { Token }
%error     { parseError }
%monad { E } { thenE } { returnE }

-- definition of tokens
%token
	finally     { TKFinally _ }
	name        { TKName _ $$ }
	string      { TKString _ $$ }
	exec        { TKExec _ }
	'=='        { TKEquals _ }
	'!='        { TKNotEquals _ }
	':'         { TKColon _ }
	';'         { TKSemicolon _ }
	'='         { TKAssignment _ }
	'{'         { TKOpenBracket _ }
	'}'         { TKCloseBracket _ }
	'-'         { TKMinus _ }

%%

-- definition of productions

Program            : ProcedureList                            { $1 }

ProcedureList      : {- empty -}                              { [] }
                   | Procedures                               { $1 }

Procedures         : Procedure                                { [$1] }
                   | Procedures Procedure                     { $1 ++ [$2] }

Procedure          : name ParameterList '-' ResultList '{'
                          GuardedCommandList
                     '}'                                      { Procedure $1 $2 $4 $6 }

ParameterList      : {- empty -}                              { [] }
                   | Parameters                               { $1 }

Parameters         : Parameter                                { [$1] }
                   | Parameters Parameter                     { $1 ++ [$2] }

Parameter          : name                                     { Param $1 }

ResultList         : {- empty -}                              { [] }
                   | Results                                  { $1 }

Results            : Result                                   { [$1] }
                   | Results Result                           { $1 ++ [$2] }

Result             : string                                   { Result $1 }

GuardedCommandList : {- empty -}                              { [] }
                   | GuardedCommands                          { $1 }

GuardedCommands    : GuardedCommand                           { [$1] }
                   | GuardedCommands GuardedCommand           { $1 ++ [$2] }

GuardedCommand     : Guards Command ';'                       { Cmd $1 $2 }

Guards             : {- empty -}                              { [] }
                   | Guards Guard ':'                         { $1 ++ [$2] }


Guard              : finally                                  { Finally }
                   | name                                     { Boolean $1 }
                   | name '==' string                         { Equals $1 $3 }
                   | name '!=' string                         { NotEquals $1 $3 }

Command            : name '=' string                          { Assignment $1 $3}
                   | name name '=' exec string string         { Execution $1 (StdOut $2) $5 (StdIn $6) }
                   | name name '=' exec string                { Execution $1 (StdOut $2) $5 (StdInNil) }
                   | name '=' exec string string              { Execution $1 (StdOutNil) $4 (StdIn $5) }
                   | name '=' exec string                     { Execution $1 (StdOutNil) $4 (StdInNil) }

{

data E a = Ok a | Failed String
	deriving Show

thenE :: E a -> (a -> E b) -> E b
m `thenE` k = 
   case m of 
       Ok a -> k a
       Failed e -> Failed e

returnE :: a -> E a
returnE a = Ok a

failE :: String -> E a
failE err = Failed err

catchE :: E a -> (String -> E a) -> E a
catchE m k = 
   case m of
      Ok a -> Ok a
      Failed e -> k e


tok f p s = f p s


-- definition of error message
parseError []     = failE ("Parse error - unexpected end of file")
parseError tokens = failE ("Parse error at row " ++ (show ((tokenGetRow (tokens!!0))+1)) ++ ", column " ++ (show ((tokenGetColumn (tokens!!0))+1)))

-- definition of data structure

data Procedure =
	Procedure Name [Parameter] [Result] [GuardedCommand]
	deriving Show

data Parameter =
	Param Name
	deriving Show

data Result =
	Result String
	deriving Show

data GuardedCommand =
	Cmd [Guard] Command
	deriving Show

data Guard =
	Boolean Name			|
	Equals Name String		|
	NotEquals Name String	|
	Finally
	deriving Show

data Command =
	Assignment Name String  |
	Execution Name ExecOutput String ExecArgument
	deriving Show

data ExecOutput =
	StdOutNil |
	StdOut Name
	deriving Show

data ExecArgument =
	StdInNil |
	StdIn String
	deriving Show

type Name = String
	
}