<?php



namespace Structure;


use Stackable;

/**
 * Class ProgProcedureCommandContext
 * @package Structure
 */
class ProgProcedureCommandContext extends Stackable
{

    /**
     * @var
     */
    protected $variable_names;
    /**
     * @var
     */
    protected $commandName;
    /**
     * @var
     */
    protected $commandParameter;
    /**
     * @var
     */
    protected $variables;

    /**
     * @return mixed
     */
    public function getCommandName()
    {
        return $this->commandName;
    }

    /**
     * @return mixed
     */
    public function getCommandParameter()
    {
        return $this->commandParameter;
    }

    /**
     * @return mixed
     */
    public function getVariableNames()
    {
        return $this->variable_names;
    }


    /**
     * @return array
     */
    public function getVariables()
    {
        return $this->variables;
    }


    /**
     * @param $variable_names
     * @param $commandName
     * @param $commandParameter
     * @param $variables
     */
    public function __construct($variable_names, $commandName, $commandParameter, $variables)
    {
        $this->variable_names = $variable_names;
        $this->commandName = $commandName;
        $this->commandParameter = $commandParameter;
        $this->variables = $variables;
    }

    /**
     *  abstract method implementation
     *  no code used because this is just a container for command
     */
    public function run()
    {
        /* this particular object won't run */
    }
} 