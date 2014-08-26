<?php


namespace Commands;


use Thread;

class Exec extends Thread
{
    public $result;

    public function __construct(&$params)
    {
        $this->result = $params;
    }

    public function run()
    {
        // run PHP exec()

        exec(
                $this->result["command"]["name"].' '.$this->result["command"]["parameter"],
                $output,
                $return_var
            );
        $this->output = $output;
            if(!$this->result[$this->result['variable_names'][0]]->isVariableBound())
                $this->result[$this->result['variable_names'][0]]->value = $return_var." ";

        if( isset($this->result['variable_names'][1]) )
        {
            if(!$this->result[$this->result['variable_names'][1]]->isVariableBound())
                $this->result[$this->result['variable_names'][1]]->value = $output[0]." ";
        }
    }
}
