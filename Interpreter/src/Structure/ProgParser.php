<?php

namespace Structure;

use Commands\Exec;
use Commands\Split;
use Exception;
use Worker;
use Structure;
use Cond;
use Mutex;

// like http://php.net/manual/en/book.pthreads.php

/**
 * Class ProgParser
 * @package Structure
 */
class ProgParser extends Worker
{
    /**
     * @var int
     */
    protected $bracecount;

    /**
     * @var array
     */
    protected $tokens;

    /**
     * @var ProgProcedureExecutionContext
     */
    protected $processContextFromParent;

    /**
     * @var ProgProcedureExecutionContext
     */
    protected $processContext;

    /**
     * @var array
     */
    protected $tokensFromParent;

    /**
     * @var array
     */
    protected $parameter;

    /**
     * @param null $code
     * @param ProgProcedureExecutionContext $processContextFromParent
     * @param array $tokensFromParent
     * @throws Exception
     */
    public function __construct($code = null, $parameter = null, ProgProcedureExecutionContext $processContextFromParent = null, array $tokensFromParent = null)
    {
        $this->bracecount = 0;

        if($code != null && $code != "")
        {
            // normal first order code call
            $this->tokens = ProgLexer::run($code);
        }
        else if ($processContextFromParent != null && $tokensFromParent != null)
        {
            // sub procedure call any level
            $this->tokensFromParent = $tokensFromParent;
            $this->processContextFromParent = $processContextFromParent;
        }
        else
        {
            throw new Exception("No code to interprete given. Exiting now.");
        }
        $this->parameter = $parameter;
    }

    /**
     * @throws Exception
     */
    public function run()
    {
        $lastNotConsumedTokens = array();

        // Parse Lexer Output
        // call commands and set up variables and strings
        foreach($this->tokens as $number => $token)
        {
            // check actual token for completness (are the braces closed...)

            switch($token)
            {

                case "T_PROCEDURE_START":
                        $this->processContext = new ProgProcedureExecutionContext();
                        $this->processContext->setProcedureName($lastNotConsumedTokens[0]['match']);
                        unset($lastNotConsumedTokens[0]);

                        foreach($lastNotConsumedTokens as $lastToken)
                        {
                            if($lastToken['name'] != "T_WHITESPACE")
                                if($lastToken['name'] == "T_IDENTIFIER")
                                {
                                    $this->processContext->addVariableName($lastToken['match']);
                                    $newVar = new ProgVariable($lastToken['match']);
                                    $this->processContext->addVariable($newVar);
                                }
                        }
                    break;

                case "T_GENERAL_COMMAND_START":
                        // assign right to left

                        // string or command
                    break;

                case "T_VARIABLE":
                        //
                        $this->synchronized(function(){
                            if ($this->Lvariable->getValue()==null) {
                                $this->wait();
                            }
                        });
                    break;
                case "T_GUARDED_COMMAND_START":
                        // last three non whitespace token are the guard

                        // if there is to the right again a : another guard

                        // if there is a ; the last stuff was a command or assignment
                    break;

                case "T_OPEN_CURLY_BRACE":
                        $this->bracecount++;
                    break;

                case "T_CLOSE_CURLY_BRACE":
                        $this->bracecount--;
                    break;

                case "T_FINALLY":

                    break;
//                case "T_STRING":
//                    if(in_array("subelement", $token))
//                    {
//                        // check parse results for deeper commands (strings containing something)
//                        // handle sub elements e.g. variables occuring in strings
//                        foreach($token["subelement"] as $subnumber => $subtoken)
//                        {
//                            //
//                        }
//                    }
//                    // does not matter
//                    break;
//                case "T_WHITESPACE":
//                    // does not matter
//                    break;
//                case "T_CONSOLE_EXEC_PARAM_START":
//                    // must be handled by other
//                    break;
//                case "T_EXEC_COMMAND":
//                    // must be handled by other
//                    break;
//                case "T_IDENTIFIER":
//                    // must be handled by other
//                    break;
//                case "T_GUARDED_COMMAND_END":
//                    // must be handled by other
//                    break;
//                case "T_EQUAL":
//                    // must be handled by other
//                    break;
//                case "T_NOT_EQUAL":
//                    // must be handled by other
//                    break;

                default:
                    $lastNotConsumedTokens[] = $token;
            }
        }



        if($this->bracecount != 0)
        {
            throw new Exception("Braces not closed correctly. Exiting with error.");
        }

        // TODO wait for finishing of all threads or check if output is already assigned

        // TODO still unbound valiables after 3 thread checks => finally

        // TODO return results
    }

}