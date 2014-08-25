<?php


namespace Commands;


use Thread;

class Exec extends Thread
{
    private $result;

    public function __construct()
    {
        // TODO handle args
    }

    public function run()
    {
        // TODO run PHP exec()
        $this->result = 'result of expensive work';
    }
}
