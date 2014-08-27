<?php


namespace Commands;

use Structure\ProgProcedureCommandContext;
use Thread;

/**
 * Class Exec
 * @package Commands
 */
class Exec extends Thread
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
     * executes a system call in a thread and sets the return values
     */
    public function run()
    {
        // run PHP exec()
        //print "   EXEC: in run";

        exec(
            $this->context->getCommandName() . ' ' . $this->context->getCommandParameter(),
            $output,
            $return_var
        );

        //print "   EXEC: in runEXEC: after system exec \n".$output[0];

        $outputVar = $this->variables[$this->variableNames[0]];

        if (isset($this->variableNames[1])) {
            $returnVar = $this->variables[$this->variableNames[1]];
        }

        if (!$outputVar->isVariableBound()) {
            if (is_array($output)) {
                $outputVar->setValue(implode(" -NEWLINE- ", $output));
            } else {
                $outputVar->setValue($output);
            }
        }

        if (!$returnVar->isVariableBound())
            $returnVar->setValue($return_var . "");

        //print "   EXEC: in runEXEC: finished \n";
    }
}
