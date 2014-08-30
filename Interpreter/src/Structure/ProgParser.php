<?php



namespace Structure;


use Commands\Exec;
use Exception;
use Worker;

// using http://php.net/manual/en/book.pthreads.php

class ProgParser extends Worker
{
    protected $bracecount;
    protected $tokens;
    protected $processContextFromParent;
    protected $tokensFromParent;

    public function __construct($code = null, ProgProcedureExecutionContext $processContextFromParent = null, array $tokensFromParent = null)
    {
        $this->bracecount = 0;

        if($code != null && $code != "")
        {
            // normal first order code call
            $this->tokens = ProgLexer::run($code);
        }
        else if ($processContextFromParent != null && $tokensFromParent != null)
        {
            // sub procedure call any level
            $this->tokensFromParent = $tokensFromParent;
            $this->processContextFromParent = $processContextFromParent;
        }
        else
        {
            throw new Exception("No code to interprete given. Exiting now.");
        }
    }

    public function run()
    {
        // Parse Lexer Output
        foreach($this->tokens as $number => $token)
        {

            // TODO check actual token for completness (are the braces closed...)

            if(in_array("subelement", $token))
            {
                // check parse results for deeper commands (strings containing something)
                // handle sub elements e.g. variables occuring in strings
                foreach($token["subelement"] as $subnumber => $subtoken)
                {
                        // TODO call sub lexer on strings containing not only [a-zA-Z0-9] (e.g. $) and replace the $with the variable if bound, else wait
                }
            }

            // TODO call commands and set up variables and strings

        }


        // TODO wait for finishing of all threads or check if output is allready assigned

        // TODO still unbound valiables after 3 thread checks => finally

        // TODO return results
    }


}