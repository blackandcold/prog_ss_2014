<?php



namespace test;


use Commands\Exec;
use Commands\Split;
use PHPUnit_Framework_TestCase;
use Structure\ProgProcedureCommandContext;
use Structure\ProgVariable;

class CommandTest extends PHPUnit_Framework_TestCase{

    // call exec as would be with 'x y = exec "cat" "test"'
    public function testExecCommand()
    {

        $externalVars = array(
            "x" => new ProgVariable("x",null),
            "y" => new ProgVariable("y",null)
        );

        /*
         * Windows Commands:
         * http://acidx.net/wordpress/2012/09/retrieving-system-information-via-command-line-on-windows/
         */
        $execContext = new ProgProcedureCommandContext(
                        array(
                                "x",
                                "y"
                        ),
                        "wmic",
                        "baseboard",
                        $externalVars
                        );


        $executor = new Exec($execContext);

        if ($executor->start())
        {
            $executor->join();

            //print_r($execContext);
        }
        // print $externalVars["x"]->getValue();
        // print $externalVars["y"]->getValue();
        $this->assertNotNull($externalVars["x"]->getValue());
        $this->assertNotNull($externalVars["y"]->getValue());
    }
    public function testExecCommandOneVar()
    {
        // TODO redo with context
        /*
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

            print_r($executor->context);
        }*/
    }

    public function testSplitCommand()
    {
        $externalVars = array(
            "x" => new ProgVariable("x",null),
            "y" => new ProgVariable("y",null)
        );

        $execContext = new ProgProcedureCommandContext(
            array(
                "x",
                "y"
            ),
            "/",
            "a/b/c",
            $externalVars
        );

        $executor = new Split($execContext);

        if ($executor->start())
        {
            $executor->join();
        }
        $this->assertNotNull($externalVars["x"]->getValue());
        $this->assertNotNull($externalVars["y"]->getValue());
        $this->assertEquals("a",$externalVars["x"]->getValue());
    }
    public function testSplitCommandMinus()
    {
        $externalVars = array(
            "x" => new ProgVariable("x",null),
            "y" => new ProgVariable("y",null)
        );

        $execContext = new ProgProcedureCommandContext(
            array(
                "x",
                "y"
            ),
            "-",
            "a-b-c",
            $externalVars
        );

        $executor = new Split($execContext);

        if ($executor->start())
        {
            $executor->join();
        }
        $this->assertNotNull($externalVars["x"]->getValue());
        $this->assertNotNull($externalVars["y"]->getValue());
        $this->assertEquals("a",$externalVars["x"]->getValue());
    }
    /*
    *   @expectedException Exception
    */
    public function testSplitCommandException()
    {
        $externalVars = array(
            "x" => new ProgVariable("x",null)
        );

        $execContext = new ProgProcedureCommandContext(
            array(
                "x"
            ),
            "/",
            "a/b/c",
            $externalVars
        );

        $executor = new Split($execContext);

        if ($executor->start())
        {
            $executor->join();
        }


        $this->assertEquals("ERROR", $externalVars["x"]->getValue());
    }
} 