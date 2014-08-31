import System.IO							-- stdio functions
import System.Console.ANSI					-- extended console output functions (color effects, cursor positions, etc.)
import Control.Concurrent(threadDelay)		-- needed for pause function
import Data.Char(ord,isControl)				-- extended functions for char data type
import Tokens								-- Alex created tokenizer
import Parser								-- Happy created parser

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

--------------------
---------------------- Entry Point - main function, program loop
--------------------

-- Entry point - initializes UI and starts I/O loop
main :: IO ()
main = do
	initScreen
	doLoop Nil

-- Recursively reads one character from STDIO and takes actions based on input:
--   TAB        = jump to command prompt
--   ENTER      = jump one editor line
--   DEL        = delete one character
--   ARROW KEYS = jump according to input
--   others     = append or overwrite character at current editor position
doLoop :: CurrentFile -> IO ()
doLoop Nil = do
	command <- readCommand
	execCommand Nil command
doLoop f@(CF n content pr pc m) = do
	refreshCurrentRow f
	c <- getChar
	case (ord c) of
		1 ->	doLoop (goTo f pr 0)
		5 ->	doLoop (goTo f pr (length (content!!pr)))
		9 ->	do
					command <- readCommand
					execCommand f command
		10 -> do
			let nf = breakLine f
			refreshRows nf [r | r <- [pr..(length content)]]
			doLoop nf
		127  -> backDelete f
		_    ->	if not (isControl c) then
					case m of
						Insert		-> doLoop (insertCharacter f c)
						Overwrite	-> doLoop (replaceCharacter f c) 

				else
					handleSpecialCharacter f [c]

--------------------
---------------------- Command Prompt Code
--------------------

-- jumps to command prompt and reads one input line
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

-- receives the current file and the string written to
-- input prompt and takes actions
execCommand :: CurrentFile -> String -> IO ()
execCommand Nil command = do
	let cmdList = wordsWhen (==' ') command
	case (cmdList!!0) of
		
		-- perform save exit to terminal
		"exit"		-> exitSave
		
		-- print a list of possible commands
		"help"		-> printHelp>>doLoop Nil
		
		-- open the file identified by the filename in the commands argument
		"open"		-> do
							fileInit <- readMyFile (cmdList!!1)
							printFileHighlighted fileInit 0
							setEditorCursorPosition (getCurPosRow fileInit) (getCurPosCol fileInit)	
							doLoop fileInit
		
		-- others: wait for a new - valid - command
		_ 			-> doLoop Nil
execCommand f@(CF _ _ _ _ _) command = do
	let cmdList = wordsWhen (==' ') command
	case (cmdList!!0) of
		
		-- perform save exit to terminal
		"exit"		-> exitSave
		
		-- print a list of possible commands
		"help"		-> printHelp>>(printFileHighlighted f 0)>>(doLoop f)
		
		-- save the current editor contents to file system
		"save"		-> (writeMyFile f)>>(doLoop f)
		
		-- jump the given row/column position
		"go"		-> doLoop (parseGoCommand f (cmdList!!1) (cmdList!!2))
		
		-- switch to insert mode
		"insert" 	-> doLoop (setWriteMode f Insert)
		
		-- switch to overwrite mode
		"overwrite"	-> doLoop (setWriteMode f Overwrite)
		
		-- delete character at current position
		"del"		-> doLoop (deleteCharacter f)
		
		-- close currently opened file
		"close"		-> initScreen>>doLoop Nil
		
		-- parse currently opened file
		"parse"		-> (printParseResults f)>>(printFileHighlighted f 0)>>(doLoop f)
		
		-- others: wait for a new - valid - commands
		_			-> doLoop f

-- parses the "go" command and sets the current editor position
parseGoCommand :: CurrentFile -> String -> String -> CurrentFile
parseGoCommand f@(CF n c pr pc m) sRow sCol = goTo f iRow iCol
	where
		iRow = read sRow :: Int
		iCol = read sCol :: Int

-- sets the current editor position to the given row/column
goTo :: CurrentFile -> Int -> Int -> CurrentFile
goTo f@(CF n c pr pc m) iRow iCol = (CF n c resultRow resultCol m)
	where
		resultRow = min (max 0 iRow) ((length c) - 1)
		resultCol = min (max 0 iCol) ((length (c!!(resultRow))))

-- adds the given number of empty rows to the editor
expandContentToLines :: FileContents -> Int -> FileContents
expandContentToLines c r = 
	if (length c <= r) then expandContentToLines (c++[""]) r
	else c

readSpecialCharacter :: CurrentFile -> [Char] -> IO ()
readSpecialCharacter f chars = do
	codeChar <- getChar
	handleSpecialCharacter f (chars++[codeChar])

-- handles special characters read from stdin
handleSpecialCharacter :: CurrentFile -> [Char] -> IO ()
handleSpecialCharacter f chars@['\ESC'] = readSpecialCharacter f chars
handleSpecialCharacter f chars@['\ESC', _] = readSpecialCharacter f chars
handleSpecialCharacter f@(CF _ _ pr pc _) chars@['\ESC', '[', c]
	| c == 'A' = refreshCurrentRow f >> doLoop (goTo f (pr-1) pc)
	| c == 'B' = refreshCurrentRow f >> doLoop (goTo f (pr+1) pc)
	| c == 'C' = refreshCurrentRow f >> doLoop (goTo f pr (pc+1))
	| c == 'D' = refreshCurrentRow f >> doLoop (goTo f pr (pc-1))
	| c == '3' = readSpecialCharacter f chars
	| otherwise = doLoop f

handleSpecialCharacter f@(CF n c pr pc m) chars@['\ESC', '[', '3', '~'] =
	if pc == length (c!!pr) then backDelete (goTo f (pr+1) 0)
	else doLoop (deleteCharacter f)
handleSpecialCharacter f _ = doLoop f

--------------------
---------------------- Basic Screen I/O
--------------------

-- initializes user interface
initScreen :: IO ()
initScreen = do
	hSetBuffering stdin NoBuffering
	clearScreen
	initCommandSection
	setEditorCursorPosition 0 0

-- repaints the command prompt section
initCommandSection :: IO ()
initCommandSection = do
	setSGR [Reset]
	setCursorPosition 0 0
	putStrLn "Command >> "
	putStrLn "--------------------------------------------------------------"

-- performs a save exit, i.e. clears the screen, sets the cursor position
-- and resets the terminals text properties to default
exitSave :: IO ()
exitSave = do
	clearScreen
	setCursorPosition 0 0
	setTitle "Terminal"
	setSGR [Reset] -- reset colors to system standard
	--getChar>>return ()

-- outputs a list of possible commands
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

-- prints the results of the parser
-- and waits for a key to be hit
printParseResults :: CurrentFile -> IO ()
printParseResults cf = printAndWait (parseFile cf)

-- generic function to print a string to the screen
-- and wait for a key to resume to editor mode
printAndWait :: String -> IO ()
printAndWait text = do
	setLineNumberCursorPosition 0
	clearFromCursorToScreenEnd
	putStr text
	getChar>>return ()
	setLineNumberCursorPosition 0
	clearFromCursorToScreenEnd

-- prints row numbers to the left of the editor area
printLineNumber :: Int -> IO ()
printLineNumber row = do
	setLineNumberCursorPosition row
	clearFromCursorToLineEnd
	putStr sRow
	where
		sRow = if (length (show row)) == 1
					then (show row) ++ " |"
					else (show row) ++ "|"

-- delays I/O for a quarter second
pause :: IO ()
pause = do
	hFlush stdout
	-- 1/4 second pause
	threadDelay 250000

-- prints the current file line by line
-- to the screen (plain text)
printFile :: CurrentFile -> IO ()
printFile (CF _ [] _ _ _)          = return ()
printFile (CF n (line:rest) pr pc m) = do
	putStrLn line
	printFile (CF n rest pr pc m)

-- prints the current file line by line
-- to the screen, started at the given row number
-- syntax highlighting is based on the lexical analysis
printFileHighlighted :: CurrentFile -> Int -> IO ()
printFileHighlighted Nil _                         = return ()
printFileHighlighted (CF _ [] _ _ _) _             = return ()
printFileHighlighted (CF n (line:rest) pr pc m)  r = do
	refreshRowHighlighted line r pr pc
	printFileHighlighted (CF n rest pr pc m) (r+1)

-- refreshes the current row, syntax highlighted
refreshCurrentRow :: CurrentFile -> IO ()
refreshCurrentRow cf = do
	refreshRowHighlighted (getCurRow cf) (getCurPosRow cf) (getCurPosRow cf) (getCurPosCol cf)

-- calls the lexical analysis and prints the
-- current row to the screen
-- erroneous rows are underlined and colored red
-- from the position of the found error
refreshRows :: CurrentFile -> [Int] -> IO ()
refreshRows f [] = return ()
refreshRows f@(CF _ c pr pc _) (x:xs) = do
	refreshRowHighlighted (c!!x) x pr pc
	refreshRows f xs

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

-- reads the file with the given filename
-- and returns a new CurrentFile representation
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

-- writes the current editor contents to the file system
-- existing files will be overwritten
writeMyFile :: CurrentFile -> IO ()
writeMyFile (CF n c _ _ _) = writeFile n (fileContentsToString c)

--------------------
---------------------- Lexer related functions
--------------------

-- lexes the current file and highlights the found tokens
lexIt :: CurrentFile -> IO ()
lexIt (CF _ fc _ _ _) = do
	let tokens = alexScanTokens (fileContentsToString fc)
	highlightTokensPaused tokens

-- receives the token list of the given row number,
-- retrieves color, intensity and value of the token
-- and prints it to the sceen
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

-- prints the erroneous in red to the console
-- from the position of the lexical error the contents
-- are underlined
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

-- syntax highlighting:
-- specifiy color of token types
tokenGetColor :: Token -> Color
tokenGetColor (TKExec _)         = Black
tokenGetColor (TKSplit _)        = Black
tokenGetColor (TKFinally _)      = Black
tokenGetColor (TKName _ v)       = Black
tokenGetColor (TKString _ v)     = Green
tokenGetColor (TKLexError _ _)   = Red
tokenGetColor _                  = Blue -- Others: operators

-- syntax highlighting:
-- specify intensity of token types
tokenGetIntensity :: Token -> ConsoleIntensity
tokenGetIntensity (TKExec _)        = BoldIntensity
tokenGetIntensity (TKSplit _)       = BoldIntensity
tokenGetIntensity (TKFinally _)     = BoldIntensity
tokenGetIntensity (TKLexError _ _)  = BoldIntensity
tokenGetIntensity _                 = NormalIntensity

-- jumps through all tokens on the screen and waits
-- for a quarter second between each token
highlightTokensPaused :: [Token] -> IO ()
highlightTokensPaused []    = return ()
highlightTokensPaused (h:t) = do
	setEditorCursorPosition (tokenGetRow h) (tokenGetColumn h)
	pause
	highlightTokensPaused t

-- invokes lexical analysis and highlights erroneous tokens, if any
lexErrors :: CurrentFile -> IO ()
lexErrors (CF _ fc _ _ _) = do
	let errors = alexScanErrors (fileContentsToString fc)
	highlightTokensPaused errors

--------------------
---------------------- Parser related functions
--------------------

-- parses the current editor contents
-- and displays a success or error message on display
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

-- convertes the file contents array to a single string
fileContentsToString :: FileContents -> String
fileContentsToString []    = ""
fileContentsToString (h:t) = h ++ "\n" ++ (fileContentsToString t)

-- jumps the current cursor position for one line
breakLine :: CurrentFile -> CurrentFile
breakLine f@(CF n c pr pc m) = (CF n nc (pr+1) 0 m)
	where
		nc = beginning ++ [firstHalf] ++ [secondHalf] ++ end
		beginning = take pr c
		firstHalf = take pc (c !! pr)
		secondHalf = drop pc (c !! pr)
		end = drop (pr+1) c

-- replaces the character at the current cursor position
-- with the given character
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

-- inserts the given character just before the current
-- cursor position
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

-- deletes the character at the current cursor position
-- and shifts the rest of the line to the left
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

-- inserts the given character in the given file row
backDelete :: CurrentFile -> IO ()
backDelete f@(CF _ _ 0 0 _) = doLoop f
backDelete f@(CF n c pr 0 m) = do
	let npr = max 0 (pr-1)
	let npc = length (c!!npr)
	let nc = (take npr c) ++ [c!!npr ++ c!!pr] ++ (drop (pr+1) c)
	let nf = (CF n nc npr npc m)
	refreshRows nf [r | r <- [npr..(length nc)-1]]
	setLineNumberCursorPosition (length nc)
	clearFromCursorToLineEnd
	doLoop nf
backDelete f@(CF n c pr pc m) = doLoop (deleteCharacter (CF n c pr (pc-1) m))

insertCharInFileRow :: FileRow -> Colnum -> Char -> FileRow
insertCharInFileRow l i n = (take i l) ++ [n] ++ (drop i l)

-- inserts the given file row in the given file contents array
insertRowInContents :: FileContents -> Rownum -> FileRow -> FileContents
insertRowInContents l i n = (take i l) ++ [n] ++ (drop i l)

-- replaces the given character in the given file row
replaceCharInFileRow :: FileRow -> Colnum -> Char -> FileRow
replaceCharInFileRow l i n = (take i l) ++ [n] ++ tail ((drop (i) l))

-- replaces the given character in the given file row
replaceRowInContents :: FileContents -> Rownum -> FileRow -> FileContents
replaceRowInContents l i n = (take i l) ++ [n] ++ tail ((drop (i) l))

-- appends the given character at the end of the given file row
appendCharInFileRow :: FileRow -> Char -> FileRow
appendCharInFileRow l n = l ++ [n]

-- deletes the character at the given position from the given file row
deleteCharInFileRow :: FileRow -> CurPosCol -> FileRow
deleteCharInFileRow l i = (take i l) ++ tail ((drop (i) l))

-- wrapper for setting the cursor position in the editor area
-- adds the desired offset from screen top/left
setEditorCursorPosition :: CurPosRow -> CurPosCol -> IO ()
setEditorCursorPosition r c = setCursorPosition (r+2) (c+3)

-- wrapper for setting the cursor position in the line number area
-- adds the desired offset from screen top/left
setLineNumberCursorPosition :: CurPosRow -> IO ()
setLineNumberCursorPosition r = setCursorPosition (r+2) 0

--------------------
---------------------- General purpose helpers
--------------------

-- splits a given string into an array
-- split position is determined by the given function
-- mapping a character to a boolean value
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =
	case dropWhile p s of
		"" -> 	[]
		s' -> 	w : wordsWhen p s''
				where (w, s'') = break p s'