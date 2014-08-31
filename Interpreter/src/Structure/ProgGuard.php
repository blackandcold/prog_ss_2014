<?php



namespace Structure;


use Thread;
use Cond;
use Mutex;

/**
 * Class ProgGuard
 * @package Structure
 */
class ProgGuard extends Thread
{
    /**
     * @var ProgVariable
     */
    protected $Lvariable;
    /**
     * @var ProgVariable
     */
    protected $Rvariable;
    /**
     * @var
     */
    protected $matchSymbol;
    /**
     * @var bool
     */
    protected $isSatisfied;

    /**
     * @return mixed
     */
    public function isSatisfied()
    {

        switch($this->matchSymbol)
        {
            case "==":
                if($this->Lvariable->getValue() == $this->Rvariable->getValue())
                {
                    $this->isSatisfied = true;
                }

                break;
            case "!=":
                if($this->Lvariable->getValue() != $this->Rvariable->getValue())
                {
                    $this->isSatisfied = true;
                }
                break;
            default:
                $this->isSatisfied = false;
        }

        return $this->isSatisfied;
    }

    /**
     * @param ProgVariable $lvariable
     * @param ProgVariable $rvariable
     * @param $matchsymbol
     */
    public function __construct(ProgVariable &$lvariable, ProgVariable &$rvariable, $matchsymbol)
    {
        $this->Lvariable = $lvariable;
        $this->Rvariable = $rvariable;
        $this->matchSymbol = $matchsymbol;
        $this->isSatisfied = false;

    }

    /**
     * abstract method implementation
     * wait for variables to get assigned
     */
    public function run()
    {
        $this->synchronized(function(){
            if ($this->Lvariable->getValue()==null) {
                $this->wait();
            }
        });

        $this->synchronized(function(){
            if ($this->Rvariable->getValue()==null) {
                $this->wait();
            }
        });

    }
} 