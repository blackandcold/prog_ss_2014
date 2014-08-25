<?php


namespace test;

use PHPUnit_Framework_TestCase;
use Structure\ProgLexer;

class TextToolsTest extends PHPUnit_Framework_TestCase
{

    public function testLexer()
    {
        $input = DataFactory::getFullMaxmumExample();

        try {
            print $input;
            $result = ProgLexer::run($input);
            print_r($result);

            $this->assertTrue(count($result) > 0);
        } catch (\Exception $e) {
            print $e->getMessage();
            $this->fail("Could not handle input source.");
        }

    }

    public function testFunctionStartToken()
    {
        $input = DataFactory::getProgramStartExample();
        print $input;
        $result = ProgLexer::run($input);
        print_r($result);
        $this->assertCount(2, $result);
    }

    public function testFunctionExtendedStartToken()
    {
        $input = DataFactory::getProgramStartExtendedExample();
        print $input;
        $result = ProgLexer::run($input);
        print_r($result);
        $this->assertCount(7, $result);
    }

    public function testFunctionExecToken()
    {
        $input = DataFactory::getExecStringExample();
        print $input;
        $result = ProgLexer::run($input);
        print_r($result);
        $this->assertCount(7, $result);

    }

    public function testFunctionSingleExecToken()
    {
        $input = DataFactory::getSingleExecExample();
        print $input;
        $result = ProgLexer::run($input);
        print_r($result);
        $this->assertCount(11, $result);
    }

    public function testFunctionSplitToken()
    {
        $input =  DataFactory::getSingleSplitExample();
        print $input;
        $result = ProgLexer::run($input);
        print_r($result);
        $this->assertCount(11, $result);
    }

    public function testVariableExample()
    {
        $input =  DataFactory::getVariableExample();
        print $input;
        $result = ProgLexer::run($input);
        print_r($result);
        $this->assertCount(6, $result);
    }

}