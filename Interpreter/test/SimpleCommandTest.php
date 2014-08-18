<?php


namespace prog\testing;


class SimpleCommandTest extends BaseTest {
    protected  $code = 'x y = exec "cat" "test"';

    // TODO test basic command class

    public function testSimpleCommandHandling()
    {
        $this->assertTrue(true);
    }
}
 