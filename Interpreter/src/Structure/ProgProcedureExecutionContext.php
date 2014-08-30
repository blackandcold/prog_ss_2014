<?php



namespace Structure;

use Stackable;

/**
 * Class ProgProcedureExecutionContext
 * @package Structure
 */
class ProgProcedureExecutionContext extends Stackable
{
    /**
     * @var
     */
    protected $procedureName;
    /**
     * @var array
     */
    protected $variableNames;
    /**
     * @var array
     */
    protected $variables;
    /**
     * @var array
     */
    protected $subprocedures;
    /**
     * @var array
     */
    protected $commands;
    /**
     * @var array
     */
    protected $guards;

    /**
     * @return mixed
     */
    public function getProcedureName()
    {
        return $this->procedureName;
    }

    /**
     * @param mixed $procedureName
     */
    public function setProcedureName($procedureName)
    {
        $this->procedureName = $procedureName;
    }
    /**
     * @return mixed
     */
    public function getCommands()
    {
        return $this->commands;
    }

    /**
     * @param mixed $command
     */
    public function setCommands($commands)
    {
        $this->commands = $commands;
    }

    /**
     * @return mixed
     */
    public function getGuards()
    {
        return $this->guards;
    }

    /**
     * @param mixed $guards
     */
    public function setGuards($guards)
    {
        $this->guards = $guards;
    }

    /**
     * @param ProgGuard $guard
     */
    public function addGuard(ProgGuard $guard)
    {
        if (!in_array($guard, $this->guards))
            $this->guards[] = $guard;
    }

    /**
     * @return mixed
     */
    public function getSubprocedures()
    {
        return $this->subprocedures;
    }

    /**
     * @param mixed $subprocedures
     */
    public function setSubprocedures($subprocedures)
    {
        $this->subprocedures = $subprocedures;
    }

    /**
     * @param ProgProcedureExecutionContext $subprocedure
     */
    public function addSubprocedur(ProgProcedureExecutionContext $subprocedure)
    {
        if (!in_array($subprocedure, $this->subprocedures))
            $this->subprocedures[] = $subprocedure;
    }

    /**
     * @return mixed
     */
    public function getVariableNames()
    {
        return $this->variableNames;
    }

    /**
     * @param mixed $variable_names
     */
    public function setVariableNames($variable_names)
    {
        $this->variableNames = $variable_names;
    }

    /**
     * @param $variableName
     */
    public function addVariableName($variableName)
    {
        if (!in_array($variableName, $this->variableNames)) {
            $this->variableNames[] = $variableName;
        }
    }

    /**
     * @return mixed
     */
    public function getVariables()
    {
        return $this->variables;
    }

    /**
     * @param mixed $variables
     */
    public function setVariables($variables)
    {
        $this->variables = $variables;
    }

    /**
     * @param ProgVariable $variable
     */
    public function addVariable(ProgVariable $variable)
    {
        if (!in_array($variable, $this->variables))
            $this->variables[] = $variable;
    }


    /**
     *
     */
    public function __construct()
    {
        $this->variableNames = array();
        $this->$variables = array();
        $this->subprocedures = array();
        $this->commands = array();
        $this->guards = array();
    }

    /**
     *
     */
    public function run()
    {
        // nothing to see here, move along
    }
} 