{
module Parser where

import Tokens
}

%name        parse
%tokentype { Token }
%error     { parseError }

%token
	finally     { TKFinally _ }
	name        { TKName _ $$ }
	string      { TKString _ $$ }
	'=='        { TKEquals _ }
	'!='        { TKNotEquals _ }
	':'         { TKColon _ }
	';'         { TKSemicolon _ }
	'='         { TKAssignment _ }

%%

GuardedCommand   : Guards ':' Command ';'         { Cmd $1 $3 }

Guards           : Guard                          { LastGuard $1 } 
                 | Guards ':' Guard               { MoreGuards $1 $3 }


Guard            : finally                        { Finally }
                 | name                           { Boolean $1 }
                 | name '==' string               { Equals $1 $3 }
                 | name '!=' string               { NotEquals $1 $3 }

Command          : name '=' string                { Assignment $1 $3}

{

tok f p s = f p s

parseError :: [Token] -> a
parseError _ = error "Parse error"

--data GuardedCommand =
--	Cmd [Guard] Command
--	deriving Show
data GuardedCommand =
	Cmd Guards Command
	deriving Show

data Guards =
	MoreGuards Guards Guard     |
	LastGuard Guard
	deriving Show

data Guard =
	Boolean Name			|
	Equals Name String		|
	NotEquals Name String	|
	Finally
	deriving Show

data Command =
	Assignment Name String
	deriving Show

type Name = String
	
}