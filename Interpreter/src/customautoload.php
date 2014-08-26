<?php

/*
 * auto loading all classes and namespaces
 * including vendor/generated from composer
 */

require 'vendor/autoload.php';

require 'DataFactory.php';

require 'Commands/Exec.php';
require 'Commands/Split.php';

require 'Structure/ProgLexer.php';
require 'Structure/ProgVariable.php';
