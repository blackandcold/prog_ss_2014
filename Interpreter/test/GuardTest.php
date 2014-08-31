<?php



namespace test;

use PHPUnit_Framework_TestCase;
use Structure\StackableArray;
use Structure\ProgVariable;
use Structure\ProgGuard;

/**
 * Class GuardTest
 * @package test
 */
class GuardTest extends PHPUnit_Framework_TestCase {

    /**
     *  testing locking of guards
     */
    public function testGuardVariableChanged()
    {
        $externalVars = array();

        $externalVars["x"] = new ProgVariable("x", "asdf");
        $externalVars["y"] = new ProgVariable("y", null);

        $guard = new ProgGuard($externalVars['x'], $externalVars['y'], "==");
        $guard->start();

        $y = $externalVars["y"];

        $y->setValue("asdf");
        $y->notify();

        $this->assertTrue($guard->isSatisfied());

    }
} 