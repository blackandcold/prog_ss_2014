<?php

use Monolog\Logger;
use Monolog\Handler\StreamHandler;

/*
 * Set up basic namespaces and classes.
 * Process input files and create handler instances
 */

// loading libraries with composer autoload
require 'customautoload.php';
// Setting up logging

// create a log channel
$log = new Logger('Prog2014');
$log->isHandling(Logger::DEBUG);

$log->pushHandler(new StreamHandler('development.log', Logger::INFO));
$log->pushHandler(new StreamHandler('development.log', Logger::NOTICE));
$log->pushHandler(new StreamHandler('development.log', Logger::WARNING));
$log->pushHandler(new StreamHandler('development.log', Logger::ERROR));
$log->pushHandler(new StreamHandler('development.log', Logger::DEBUG));


// TODO start code utils (not threaded)

// TODO create program class (threaded)
    // TODO create procedures (threaded)
        // TODO create guards (threaded)
            // TODO create commands (threaded)