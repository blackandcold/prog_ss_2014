<?php

/*
 * auto loading all classes and namespaces
 * including vendor/generated from composer
 */

require_once 'vendor/autoload.php';

require_once 'DataFactory.php';

require_once 'Commands/Exec.php';
require_once 'Commands/Split.php';

require_once 'Structure/ProgLexer.php';
require_once 'Structure/ProgVariable.php';
require_once 'Structure/ProgGuard.php';
require_once 'Structure/ProgParser.php';
require_once 'Structure/ProgProcedureExecutionContext.php';
require_once 'Structure/ProgProcedureCommandContext.php';
