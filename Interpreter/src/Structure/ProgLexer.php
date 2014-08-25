<?php



namespace Structure;

// Lexer constructed after http://nitschinger.at/Writing-a-simple-lexer-in-PHP

class ProgLexer
{
    //
    protected static $allowedtokens = array(

        "/^(\s-\s)/" => "T_FUNCTIONSTART",
        "/^(\s-([a-z]+))/" => "T_CONSOLEEXECPARAMSTART",
        "/^(=)/" => "T_COMMANDSTART",
        "/^(exec)/" => "T_EXECCOMMAND",
        "/^(split)/" => "T_SPLITCOMMAND",
        "/^(\\\"[^\\\"]+\\\")/" => "T_STRING",
        "/^([a-zA-Z0-9]+)/" => "T_IDENTIFIER",
        "/^(\\\$[a-z]\\\$)/" => "T_VARIABLE",
        "/^(:)/" => "T_GUARDEDCOMMANDSTART",
        "/^(;)/" => "T_GUARDEDCOMMANDENDTOKEN",
        "/^({)/" => "T_BLOCKOPEN",
        "/^(})/" => "T_BLOCKCLOSE",
        "/^(==)/" => "T_EQUAL",
        "/^(!=)/" => "T_NOTEQUAL",
        "/^(finally)/" => "T_FINALLY",
        "/^(\s+)/" => "T_WHITESPACE",
    );

    public static function run($code)
    {
        if(!is_array($code))
        {
            $code = explode("\n", $code);
        }
        $tokens = array();

        foreach($code as $number => $line)
        {
            $offset = 0;


            while($offset < strlen($line))
            {
                $result = static::find($line, $number, $offset);

                if($result === false)
                {
                    throw new \Exception("Unable to parse line " . ($line+1) . ".");
                }

                $tokens[] = $result;
                $offset += strlen($result['match']);
            }
        }

        return $tokens;
    }
    protected static function find($line, $number, $offset)
    {
        $string = substr($line, $offset);

        foreach(static::$allowedtokens as $pattern => $name)
        {
            if(preg_match($pattern, $string, $matches))
            {
                return array(
                    'match' => $matches[1],
                    'token' => $name,
                    'line' => $number+1
                );
            }
        }

        return false;
    }
} 