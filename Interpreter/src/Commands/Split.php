<?php


namespace Commands;


use Thread;

class Split extends Thread{

    public $result;

    public function __construct()
    {
        // TODO handle args
    }

    public function run()
    {
        // TODO run PHP split on vars
        $this->result = 'result of expensive work';
    }

} 