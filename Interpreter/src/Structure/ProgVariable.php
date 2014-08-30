<?php



namespace Structure;

use Stackable;


/**
 * Class ProgVariable
 * @package Structure
 */
class ProgVariable extends Stackable
{
    /**
     * @var
     */
    private $value;

    /**
     * @var
     */
    private $name;


    /**
     * @param $name
     * @param null $value
     * @throws \Exception
     */
    public function __construct($name, $value = null)
    {
        $this->name = $name;
        if (!is_string($value) && !is_null($value))
            throw new \Exception("Value of Variable must be a String!");
        $this->value = $value;
    }

    /**
     * @return mixed
     */
    public function getName()
    {
        return $this->name;
    }

    /**
     * @param mixed $name
     */
    public function setName($name)
    {
        $this->name = $name;
    }

    /**
     * @return mixed
     */
    public function getValue()
    {
        return $this->value;
    }

    /**
     * @param mixed $value
     */
    public function setValue($value)
    {
        $this->value = $value;
    }

    /**
     *  abstract method implementation
     * no code used because this is just a container for variables
     */
    public function run()
    {
        /* this particular object won't run */
    }

    /**
     * @return bool
     */
    public function isVariableBound()
    {
        return is_null($this->value) ? false : true;
    }
} 