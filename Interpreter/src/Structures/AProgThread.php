<?php


namespace prog\structures;

abstract class AProgThread implements IProgThread {

    public function __construct($threadId)
    {
        $this->threadId = $threadId;
    }

    public abstract function run();
} 