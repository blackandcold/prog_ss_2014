<?php


namespace prog\structures\impl;


use prog\structures\AProgThread;
use prog\structures\ICommand;

class Command extends AProgThread implements ICommand{

    protected $threadId;
    protected $variables;
    protected $operations;

    public function __construct($threadId)
    {
        if($threadId == null)
            $this->threadId = md5(time()+rand(1,99999));


        else
            $this->threadId = $threadId;
    }

    public function run()
    {
        // TODO: Implement run() method.
    }
}