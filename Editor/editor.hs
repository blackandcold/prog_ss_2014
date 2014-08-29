import System.IO
import System.Console.ANSI
import Control.Concurrent
import Data.Char(ord,isControl)
import Tokens
import Parser

--------------------
---------------------- Custom data types + getters and setters
--------------------
type Filename     = String
type CurPosRow    = Int
type CurPosCol    = Int
type FileRow      = [Char]
type FileContents = [FileRow]
type Rownum       = Int
type Colnum       = Int
data WriteMode    = Insert | Overwrite
data CurrentFile  = Nil |
					CF Filename FileContents CurPosRow CurPosCol WriteMode

getFileName     (CF n c pr pc m) = n
getFileContents (CF n c pr pc m) = c
getCurPosRow    (CF n c pr pc m) = pr
getCurPosCol    (CF n c pr pc m) = pc
getCurRow       (CF n c pr pc m) = c!!pr
getWriteMode    (CF n c pr pc m) = m
setFileName     (CF n c pr pc m) newval = (CF newval c pr pc m)
setFileContents (CF n c pr pc m) newval = (CF n newval pr pc m)
setCurPosRow    (CF n c pr pc m) newval = (CF n c newval pc m)
setCurPosCol    (CF n c pr pc m) newval = (CF n c pr newval m)
setWriteMode    (CF n c pr pc m) newval = (CF n c pr pc newval)

--cfInit = (CF "file.test" ["Zeile1", "Zeile2", "Zeile3"] 1 1 Insert)

--------------------
---------------------- Entry Point - main function, program loop
--------------------

main :: IO ()
main = do
	initScreen
	{-clearScreen
	fileInit <- readMyFile "test_guard.txt"
	printFileHighlighted fileInit 0
	setEditorCursorPosition (getCurPosRow fileInit) (getCurPosCol fileInit)
	putStrLn (parseFile fileInit)
	setSGR [Reset]-}
	doLoop Nil

doLoop :: CurrentFile -> IO ()
doLoop Nil = do
	command <- readCommand
	execCommand Nil command
doLoop f@(CF n content pr pc m) = do
	refreshCurrentRow f
	c <- getChar
	case (ord c) of
		9 ->	do
					command <- readCommand
					execCommand f command
		10 -> doLoop (jumpLine f)
		127  -> if pc == 0 then doLoop f
				else doLoop (deleteCharacter (CF n content pr (pc-1) m))
		_    ->	if not (isControl c) then
					case m of
						Insert		-> doLoop (insertCharacter f c)
						Overwrite	-> doLoop (replaceCharacter f c) 

				else do
					codeChar <- getChar
					if (codeChar == '[') then do
						codeChar <- getChar
						if (codeChar == '3') then do
							codeChar <- getChar
							doLoop (handleSpecialCharacter f codeChar)
						else do
							refreshCurrentRow f
							doLoop (handleSpecialCharacter f codeChar)
					else
						doLoop (handleSpecialCharacter f codeChar)

--------------------
---------------------- Command Prompt Code
--------------------

readCommand :: IO String
readCommand = do
	setCursorPosition 0 0
	setSGR [Reset]
	setSGR [SetConsoleIntensity BoldIntensity]
	setSGR [SetSwapForegroundBackground True]
	putStrLn "Command >>                                                    "
	setCursorPosition 0 11
	setSGR [Reset]
	command <- getLine
	setCursorPosition 0 0
	clearFromCursorToLineEnd
	putStr "Command >> "
	return command

execCommand :: CurrentFile -> String -> IO ()
execCommand Nil command = do
	let cmdList = wordsWhen (==' ') command
	case (cmdList!!0) of
		"exit"		-> exitSave
		"help"		-> printHelp>>doLoop Nil
		"open"		-> do
							fileInit <- readMyFile (cmdList!!1)
							printFileHighlighted fileInit 0
							setEditorCursorPosition (getCurPosRow fileInit) (getCurPosCol fileInit)	
							doLoop fileInit
		_ 			-> doLoop Nil
execCommand f@(CF _ _ _ _ _) command = do
	let cmdList = wordsWhen (==' ') command
	case (cmdList!!0) of
		"exit"		-> exitSave
		"help"		-> printHelp>>(printFileHighlighted f 0)>>(doLoop f)
		"save"		-> (writeMyFile f)>>(doLoop f)
		"go"		-> doLoop (parseGoCommand f (cmdList!!1) (cmdList!!2))
		"insert" 	-> doLoop (setWriteMode f Insert)
		"overwrite"	-> doLoop (setWriteMode f Overwrite)
		"del"		-> doLoop (deleteCharacter f)
		"close"		-> initScreen>>doLoop Nil
		"parse"		-> (printParseResults f)>>(printFileHighlighted f 0)>>(doLoop f)
		_			-> doLoop f

parseGoCommand :: CurrentFile -> String -> String -> CurrentFile
parseGoCommand f@(CF n c pr pc m) sRow sCol = goTo f iRow iCol
	where
		iRow = read sRow :: Int
		iCol = read sCol :: Int

goTo :: CurrentFile -> Int -> Int -> CurrentFile
goTo f@(CF n c pr pc m) iRow iCol = (CF n nc resultRow resultCol m)
	where
		nc = expandContentToLines c iRow
		resultRow = min (max 0 iRow) ((length nc) - 1)
		resultCol = min (max 0 iCol) ((length (nc!!(resultRow))))

expandContentToLines :: FileContents -> Int -> FileContents
expandContentToLines c r = 
	if (length c <= r) then expandContentToLines (c++[""]) r
	else c

handleSpecialCharacter :: CurrentFile -> Char -> CurrentFile
handleSpecialCharacter f@(CF n c pr pc m) ch =
	case ch of
		'A'			-> goTo f (pr-1) pc
		'B'			-> goTo f (pr+1) pc
		'C'			-> goTo f pr (pc+1)
		'D'			-> goTo f pr (pc-1)
		'~'			-> deleteCharacter f
		_			-> f

--------------------
---------------------- Basic Screen I/O
--------------------

initScreen :: IO ()
initScreen = do
	hSetBuffering stdin NoBuffering
	clearScreen
	initCommandSection
	setEditorCursorPosition 0 0

initCommandSection :: IO ()
initCommandSection = do
	setSGR [Reset]
	setCursorPosition 0 0
	putStrLn "Command >> "
	putStrLn "--------------------------------------------------------------"

exitSave :: IO ()
exitSave = do
	clearScreen
	setCursorPosition 0 0
	setTitle "Terminal"
	setSGR [Reset] -- reset colors to system standard
	--getChar>>return ()

printHelp :: IO ()
printHelp =
	printAndWait "\
\ >> open <filename>        Open file with name <filename>\n\
\ >> save                   Save current file file\n\
\ >> close                  Close current file\n\
\ \n\
\ >> parse                  Parse file\n\
\ \n\
\ >> go <rowNum> <colNum>   Set cursor to <rowNum>, <colNum>\n\
\ >> del                    Delete character at current cursor position\n\
\ >> insert                 Switch to insert mode\n\
\ >> overwrite              Switch to overwrite mode\n\
\ \n\
\ >> help                   View help\n\
\ >> exit                   Exit without save\n\
\ \n\
\ Press any key to resume..."

printParseResults :: CurrentFile -> IO ()
printParseResults cf = printAndWait (parseFile cf)

printAndWait :: String -> IO ()
printAndWait text = do
	setLineNumberCursorPosition 0
	clearFromCursorToScreenEnd
	putStr text
	getChar>>return ()
	setLineNumberCursorPosition 0
	clearFromCursorToScreenEnd

printLineNumber :: Int -> IO ()
printLineNumber row = do
	setLineNumberCursorPosition row
	clearFromCursorToLineEnd
	putStr sRow
	where
		sRow = if (length (show row)) == 1
					then (show row) ++ " |"
					else (show row) ++ "|"

pause :: IO ()
pause = do
	hFlush stdout
	-- 1/4 second pause
	threadDelay 250000

printFile :: CurrentFile -> IO ()
printFile (CF _ [] _ _ _)          = return ()
printFile (CF n (line:rest) pr pc m) = do
	putStrLn line
	printFile (CF n rest pr pc m)

printFileHighlighted :: CurrentFile -> Int -> IO ()
printFileHighlighted Nil _                         = return ()
printFileHighlighted (CF _ [] _ _ _) _             = return ()
printFileHighlighted (CF n (line:rest) pr pc m)  r = do
	refreshRowHighlighted line r pr pc
	printFileHighlighted (CF n rest pr pc m) (r+1)

refreshCurrentRow :: CurrentFile -> IO ()
refreshCurrentRow cf = do
	refreshRowHighlighted (getCurRow cf) (getCurPosRow cf) (getCurPosRow cf) (getCurPosCol cf)

refreshRowHighlighted :: FileRow -> Int -> CurPosRow -> CurPosCol -> IO ()
refreshRowHighlighted fr r pr pc = do
	let errors = alexScanErrors fr
	    tokens = alexScanTokens fr
	setSGR [Reset]
	printLineNumber r
	if null errors
		then do
			    -- repaint whole line in system default to get rid of underlinements of whitespaces
				setEditorCursorPosition r 0
				putStr fr
				-- repaint highlighted tokens
				highlightTokens tokens r
		else highlightErrors fr errors r
	setEditorCursorPosition pr pc

--------------------
---------------------- Basic File System I/O
--------------------

readMyFile :: Filename -> IO CurrentFile
readMyFile n = do
	contents <- readFile n
	setTitle ("Editor.hs => " ++ n)
	let listedContents = wordsWhen (=='\n') contents
	return 
		(
			CF
			n
			listedContents
			0--((length listedContents)-1) -- last row
			0--(length (listedContents!!((length listedContents)-1))) -- one column after last char in last row
			Insert
		)

writeMyFile :: CurrentFile -> IO ()
writeMyFile (CF n c _ _ _) = writeFile n (fileContentsToString c)

--------------------
---------------------- Lexer related functions
--------------------

lexIt :: CurrentFile -> IO ()
lexIt (CF _ fc _ _ _) = do
	let tokens = alexScanTokens (fileContentsToString fc)
	highlightTokensPaused tokens

highlightTokens :: [Token] -> Int -> IO ()
highlightTokens [] _    = return ()
highlightTokens (h:t) r = do
	setEditorCursorPosition r (tokenGetColumn h)
	let color     = tokenGetColor h
	    intensity = tokenGetIntensity h
	    value     = tokenGetValue h
	setSGR [SetColor Foreground Dull color]
	setSGR [SetConsoleIntensity intensity]
	putStr value
	highlightTokens t r

highlightErrors :: FileRow -> [Token] -> Int -> IO ()
highlightErrors _ [] _     = return ()
highlightErrors fr (h:t) r = do
	let color     = tokenGetColor h
	    intensity = tokenGetIntensity h
	    v         = tokenGetValue h
	setSGR [SetColor Foreground Dull color]
	setSGR [SetConsoleIntensity intensity]
	setEditorCursorPosition r 0
	putStr fr
	setEditorCursorPosition r (tokenGetColumn h)
	setSGR [SetUnderlining SingleUnderline]
	putStr v
	setSGR [SetUnderlining NoUnderline]
	highlightErrors fr t r

tokenGetColor :: Token -> Color
tokenGetColor (TKExec _)         = Black
tokenGetColor (TKSplit _)        = Black
tokenGetColor (TKFinally _)      = Black
tokenGetColor (TKName _ v)       = Black
tokenGetColor (TKString _ v)     = Green
tokenGetColor (TKLexError _ _)   = Red
tokenGetColor _                  = Blue -- Others: operators

tokenGetIntensity :: Token -> ConsoleIntensity
tokenGetIntensity (TKExec _)        = BoldIntensity
tokenGetIntensity (TKSplit _)       = BoldIntensity
tokenGetIntensity (TKFinally _)     = BoldIntensity
tokenGetIntensity (TKLexError _ _)  = BoldIntensity
tokenGetIntensity _                 = NormalIntensity

highlightTokensPaused :: [Token] -> IO ()
highlightTokensPaused []    = return ()
highlightTokensPaused (h:t) = do
	setEditorCursorPosition (tokenGetRow h) (tokenGetColumn h)
	pause
	highlightTokensPaused t

lexErrors :: CurrentFile -> IO ()
lexErrors (CF _ fc _ _ _) = do
	let errors = alexScanErrors (fileContentsToString fc)
	highlightTokensPaused errors

--------------------
---------------------- Parser related functions
--------------------

parseFile :: CurrentFile -> String
parseFile (CF _ fc _ _ _) =
	case parseResult of
		Failed msg -> msg
		_          -> "No parse errors detected"
	where
		tokens = alexScanTokens (fileContentsToString fc)
		parseResult = parse tokens

--------------------
---------------------- Editor related functions
--------------------

fileContentsToString :: FileContents -> String
fileContentsToString []    = ""
fileContentsToString (h:t) = h ++ "\n" ++ (fileContentsToString t)

jumpLine :: CurrentFile -> CurrentFile
jumpLine f@(CF n c pr pc m) = goTo f (pr+1) pc

replaceCharacter :: CurrentFile -> Char -> CurrentFile
replaceCharacter f@(CF n c pr pc m) ch =
	CF
	n
	(
		replaceRowInContents
			c
			(getCurPosRow f)
			(if (getCurPosCol f) < (length (getCurRow f))
				then replaceCharInFileRow (getCurRow f) (getCurPosCol f) ch
				else appendCharInFileRow (getCurRow f) ch)
	)
	pr
	(pc+1)
	m

insertCharacter :: CurrentFile -> Char -> CurrentFile
insertCharacter f@(CF n c pr pc m) ch =
	CF
	n
	(
		replaceRowInContents
			c
			(getCurPosRow f)
			(if (getCurPosCol f) < (length (getCurRow f))
				then insertCharInFileRow (getCurRow f) (getCurPosCol f) ch
				else appendCharInFileRow (getCurRow f) ch)
	)
	pr
	(pc+1)
	m

deleteCharacter :: CurrentFile -> CurrentFile
deleteCharacter f@(CF n c pr pc m) =
	CF
	n
	(
		replaceRowInContents
			c
			(getCurPosRow f)
			(if (getCurPosCol f) < (length (getCurRow f))
				then deleteCharInFileRow (getCurRow f) (getCurPosCol f)
				else getCurRow f)
	)
	pr
	pc
	m

insertCharInFileRow :: FileRow -> Colnum -> Char -> FileRow
insertCharInFileRow l i n = (take i l) ++ [n] ++ (drop i l)

insertRowInContents :: FileContents -> Rownum -> FileRow -> FileContents
insertRowInContents l i n = (take i l) ++ [n] ++ (drop i l)

replaceCharInFileRow :: FileRow -> Colnum -> Char -> FileRow
replaceCharInFileRow l i n = (take i l) ++ [n] ++ tail ((drop (i) l))

replaceRowInContents :: FileContents -> Rownum -> FileRow -> FileContents
replaceRowInContents l i n = (take i l) ++ [n] ++ tail ((drop (i) l))

appendCharInFileRow :: FileRow -> Char -> FileRow
appendCharInFileRow l n = l ++ [n]

deleteCharInFileRow :: FileRow -> CurPosCol -> FileRow
deleteCharInFileRow l i = (take i l) ++ tail ((drop (i) l))

setEditorCursorPosition :: CurPosRow -> CurPosCol -> IO ()
setEditorCursorPosition r c = setCursorPosition (r+2) (c+3)

setLineNumberCursorPosition :: CurPosRow -> IO ()
setLineNumberCursorPosition r = setCursorPosition (r+2) 0

--------------------
---------------------- General purpose helpers
--------------------

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =
	case dropWhile p s of
		"" -> 	[]
		s' -> 	w : wordsWhen p s''
				where (w, s'') = break p s'