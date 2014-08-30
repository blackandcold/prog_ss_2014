<?php



namespace Structure;


use Stackable;

class ProgGuard extends Stackable
{
    protected $Lvariable;
    protected $Rvariable;
    protected $matchSymbol;
    protected $isSatisfied;

    /**
     * @return mixed
     */
    public function isSatisfied()
    {
        if($this->Lvariable->isVariableBound() && $this->Rvariable->isVariableBound())
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

        }

        return $this->isSatisfied;
    }

    public function __construct(ProgVariable &$lvariable, ProgVariable &$rvariable, $matchsymbol)
    {
        $this->Lvariable = $lvariable;
        $this->Rvariable = $rvariable;
        $this->matchSymbol = $matchsymbol;
        $this->isSatisfied = false;
    }

    /**
     * abstract method implementation
     * checking is variables are bound
     * then check guard
     */
    public function run()
    {

    }
} 