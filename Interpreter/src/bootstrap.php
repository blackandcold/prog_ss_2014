<?php

use Monolog\Logger;

/*
 * Set up basic namespaces and classes.
 * Process input files and create handler instances
 */

// loading libraries with composer autoload
require 'customautoload.php';
// Setting up logging

// create a log channel
$log = new Logger('Prog2014');

$log->pushHandler(new \Monolog\Handler\ErrorLogHandler());


// TODO start code utils (not threaded)

// TODO create program class (threaded)
// TODO create procedures (threaded)
// TODO create guards (threaded)
// TODO create commands (threaded)