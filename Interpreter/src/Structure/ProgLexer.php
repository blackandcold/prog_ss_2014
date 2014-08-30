<?php



namespace Structure;


/**
 * Class ProgLexer
 * @package Structure
 * inspired by http://nitschinger.at/Writing-a-simple-lexer-in-PHP
 */
class ProgLexer
{
    //
    /**
     * @var array
     */
    protected static $allowedtokens = array(

        "/^(\s-\s)/" => "T_PROCEDURE_START",
        "/^(\s-([a-z]+))/" => "T_CONSOLE_EXEC_PARAM_START",
        "/^(=)/" => "T_GENERAL_COMMAND_START",
        "/^(exec)/" => "T_EXEC_COMMAND",
        "/^(split)/" => "T_SPLIT_COMMAND",
        "/^(\\\"[^\\\"]+\\\")/" => "T_STRING",
        "/^([a-zA-Z0-9]+)/" => "T_IDENTIFIER",
        "/^(\\\$[a-z]+\\\$)/" => "T_VARIABLE",
        "/^(:)/" => "T_GUARDED_COMMAND_START",
        "/^(;)/" => "T_GUARDED_COMMAND_END",
        "/^({)/" => "T_OPEN_CURLY_BRACE",
        "/^(})/" => "T_CLOSE_CURLY_BRACE",
        "/^(==)/" => "T_EQUAL",
        "/^(!=)/" => "T_NOT_EQUAL",
        "/^(finally)/" => "T_FINALLY",
        "/^(\s+)/" => "T_WHITESPACE",
    );

    /**
     * @param $code
     * @return array
     * @throws \Exception
     */
    public static function run($code)
    {
        if (!is_array($code))
        {
            $code = explode("\n", $code);
        }
        $tokens = array();

        foreach ($code as $number => $line)
        {
            $offset = 0;


            while ($offset < strlen($line))
            {
                $result = static::find($line, $number, $offset);

                if ($result === false)
                {
                    throw new \Exception("Unable to parse line " . ($line + 1) . ".");
                }
                else
                {
                    if($result['token'] == "T_STRING" && strpos($result['match'],'$') !== false )
                    {
                        $subelement = trim($result['match'], "\"");
                        $result['subelement'] = ProgLexer::run($subelement);
                    }
                }

                $tokens[] = $result;
                $offset += strlen($result['match']);
            }
        }

        return $tokens;
    }

    /**
     * @param $line
     * @param $number
     * @param $offset
     * @return array|bool
     */
    protected static function find($line, $number, $offset)
    {
        $string = substr($line, $offset);

        foreach (static::$allowedtokens as $pattern => $name)
        {
            if (preg_match($pattern, $string, $matches))
            {
                return array(
                    'match' => $matches[1],
                    'token' => $name,
                    'line' => $number + 1
                );
            }
        }

        return false;
    }
} 