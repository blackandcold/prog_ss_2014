<?php



namespace Structure;


/**
 * Class ProgVariable
 * @package Structure
 */
class ProgVariable {
    /**
     * @var
     */
    public $value;

    /**
     * @var
     */
    public $name;


    /**
     * @param $name
     * @param null $value
     * @throws \Exception
     */
    public function __construct($name, $value = null)
    {
        $this->name = $name;
        if(!is_string($value) && !is_null($value))
            throw new \Exception("Value of Variable must be a String!");
        $this->value = $value;
    }

    /**
     * @return bool
     */
    public function isVariableBound()
    {
        return is_null($this->value) ? false : true;
    }
} 