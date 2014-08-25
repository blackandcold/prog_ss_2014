<?php



namespace Structure;


use Commands\Exec;
use Thread;

// using http://php.net/manual/en/book.pthreads.php

class ProgParser extends Thread{

    public function run()
    {
        // TODO Parse Lexer Output
            // TODO check actual token for completness (are the braces closed...)
            // TODO call commands and set up variables and strings
                // TODO call sub lexer on strings containing not only [a-zA-Z0-9]


        // TODO wait for finishing of all threads
            // TODO still unbound valiables => finally or exception

        // TODO return results
    }

    public function runExec()
    {
        // TODO PASS ARGS
        $execThread = new Exec();

        if ($execThread->start())
        {
            /*
             * Do some expensive work, while already doing other
             * work in the child thread.
             */

            // wait until thread is finished
            $execThread->join();

            // we can now even access $thread->data
        }
    }



} 