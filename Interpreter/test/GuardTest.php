<?php



namespace test;

use PHPUnit_Framework_TestCase;
use Structure\StackableArray;
use Structure\ProgVariable;
use Structure\ProgGuard;

class GuardTest extends PHPUnit_Framework_TestCase {

    public function testGuardVariableChanged()
    {
        $externalVars = array();

        $externalVars["x"] = new ProgVariable("x", "asdf");
        $externalVars["y"] = new ProgVariable("y", null);

        $guard = new ProgGuard($externalVars['x'], $externalVars['y'], "==");

        $this->assertTrue(!$guard->isSatisfied());

        $externalVars['y']->setValue("asdf");


        $this->assertTrue($guard->isSatisfied());

    }
} 