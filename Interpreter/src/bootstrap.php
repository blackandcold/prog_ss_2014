<?php

use Monolog\Logger;
use Monolog\Handler\StreamHandler;
use Ulrichsg\Getopt\Getopt;
use Ulrichsg\Getopt\Option;

/*
 * Set up basic namespaces and classes.
 * Process input files and create handler instances
 */

// loading libraries with composer autoload
require 'vendor/autoload.php';

// Setting up logging

// create a log channel
$log = new Logger('Prog2014');
$log->isHandling(Logger::DEBUG);

$log->pushHandler(new StreamHandler('development.log', Logger::INFO));
$log->pushHandler(new StreamHandler('development.log', Logger::NOTICE));
$log->pushHandler(new StreamHandler('development.log', Logger::WARNING));
$log->pushHandler(new StreamHandler('development.log', Logger::ERROR));
$log->pushHandler(new StreamHandler('development.log', Logger::DEBUG));

/*
 * Command line argument parsing
 */

$arguments = null;

try{
$getopt = new Getopt(array(
        new Option('f', 'file', Getopt::REQUIRED_ARGUMENT, 'Specify an input source code file'),
        new Option('d', 'debug', Getopt::NO_ARGUMENT, 'Define log level to debug'),
        new Option('h', 'help', Getopt::NO_ARGUMENT, 'Display help text before executing source')
    ));

$getopt->parse();
}
catch (\UnexpectedValueException $e)
{
    echo $getopt->getHelpText();
    $log->addError("Code ".$e->getCode().": ".$e->getMessage());
}
catch(\InvalidArgumentException $e)
{
    echo $getopt->getHelpText();
    $log->addError("Code ".$e->getCode().": ".$e->getMessage());
}
catch(\Exception $e)
{
    echo "Unknown Error.\n\n";
    $log->addError("Code ".$e->getCode().": ".$e->getMessage());
}

if(count($getopt) == 0)
{
    echo $getopt->getHelpText();
    die();
}

if($d = $getopt->getOption("d"))
{
    $log->notice("Debug flag set");
}
else
{
    $log->isHandling(Logger::WARNING);
    $log->notice("Debug flag not set, only warning and error logged");
}

if($filepath = $getopt->getOption("d"))
{
    // TODO fetch input file
}

// TODO start code utils (not threaded)

// TODO create program class (threaded)
    // TODO create procedures (threaded)
        // TODO create guards (threaded)
            // TODO create commands (threaded)