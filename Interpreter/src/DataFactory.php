<?php



namespace test;


class DataFactory
{

    public static function getProgramStartExample()
    {
        // TESTCASE don't change
        return "asdf - ";
    }

    public static function getProgramStartExtendedExample()
    {
        // TESTCASE don't change
        return 'asdf - "$max$" { }';
    }

    public static function getExecStringExample()
    {
        // TESTCASE don't change
        return 'ab = exec "test $a$ -le $b$"';
    }

    public static function getSingleExecExample()
    {
        // TESTCASE don't change
        return 'x y = exec "cat" "test"';
    }

    public static function getSingleSplitExample()
    {
        // TESTCASE don't change
        return 'x y = split "/" "a/b/c"';
    }

    public static function getFullMaxnumExample()
    {
        // TESTCASE don't change
        return 'maxnum a b c - "$max$" {
ab = exec "test $a$ -le $b$";
bc = exec "test $b$ -le $c$";
ab == "0" : bc == "0" : max = "$c$";
ab == "0" : bc == "1" : max = "$b$";
ab == "1" : bc == "1" : max = "$a$";
ab == "1" : bc == "0" : ac = exec "test $a$ -le $c$";
ac == "0" : max = "$c$";
ac == "1" : max = "$a$";
finally : max = "error";
                }';
    }

    public static function getVariableExample()
    {
        // TESTCASE don't change
        return 'test $a$ -le $c$';
    }

    /* not yet functioning or incomplete cases */

    public static function getFullCustomExample()
    {
        // TODO filename listing subdirs or something
        return '';
    }
} 