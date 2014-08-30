<?php



namespace Structure;


use Commands\Exec;
use Worker;

// using http://php.net/manual/en/book.pthreads.php

class ProgParser extends Worker
{
    protected $bracecount;

    public function __construct()
    {
        $this->bracecount = 0;
    }

    public function run()
    {
        // TODO Parse Lexer Output
        // TODO check actual token for completness (are the braces closed...)

        // TODO check parse results for deeper commands (strings containing something or sup procedures)

        // TODO call commands and set up variables and strings
        // TODO call sub lexer on strings containing not only [a-zA-Z0-9] (e.g. $) and replace the $with the variable if bound, else wait$


        // TODO wait for finishing of all threads
        // TODO still unbound valiables after 3 thread checks => finally

        // TODO return results
    }

}