<?php



namespace test;


use Commands\Exec;
use PHPUnit_Framework_TestCase;
use Structure\ProgVariable;

class CommandTest extends PHPUnit_Framework_TestCase{

    // call exec as would be with 'x y = exec "cat" "test"'
    public function testExecCommand()
    {
        $vars = array(
                "variable_names" => array(
                                            "x",
                                            "y"
                                        ),
                "command" => array(
                                    "name" => "sysctl",
                                    "parameter" => "-n machdep.cpu.brand_string"
                                    ),
                "x" => new ProgVariable("x",null),
                "y" => new ProgVariable("y",null)
        );

        $executor = new Exec($vars);

        if ($executor->start())
        {
            $executor->join();

            print_r($executor->result);
        }
    }
    public function testExecCommandOneVar()
    {
        $vars = array(
            "variable_names" => array(
                "x"
            ),
            "command" => array(
                "name" => "sysctl",
                "parameter" => "-n machdep.cpu.features "
            ),
            "x" => new ProgVariable("x",null)
        );
        $executor = new Exec($vars);

        if ($executor->start())
        {
            $executor->join();

            print_r($executor->result);
        }
    }
} 