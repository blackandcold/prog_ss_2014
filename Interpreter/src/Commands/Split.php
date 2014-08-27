<?php


namespace Commands;

use Structure\ProgProcedureCommandContext;
use Thread;

/**
 * Class Split
 * @package Commands
 */
class Split extends Thread
{

    /**
     * @var
     */
    public $context;

    /**
     * @var array
     */
    private $variables;
    /**
     * @var mixed
     */
    private $variableNames;

    /**
     * @param $params
     */
    public function __construct(ProgProcedureCommandContext $context)
    {
        $this->context = $context;
        $this->variables = $context->getVariables();
        $this->variableNames = $context->getVariableNames();
    }

    /**
     *
     */
    public function run()
    {
        $firstVar = $this->variables[$this->variableNames[0]];

        if (isset($this->variableNames[1])) {
            $secondVar = $this->variables[$this->variableNames[1]];
        } else {
            if (!$firstVar->isVariableBound())
                $firstVar->setValue("ERROR");
            return;
        }

        $pieces = explode($this->context->getCommandName(), $this->context->getCommandParameter());


        if (!$firstVar->isVariableBound())
            $firstVar->setValue($pieces[0]);
        else {
            // ignore
        }
        $pieces = array_slice($pieces, 1);

        if (!$secondVar->isVariableBound())
            $secondVar->setValue(implode($this->context->getCommandName(), $pieces));
        else {
            // ignore
        }
    }

} 