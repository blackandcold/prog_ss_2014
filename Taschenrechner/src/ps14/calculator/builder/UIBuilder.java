package ps14.calculator.builder;

import static ps14.calculator.builder.Builder.If;
import static ps14.calculator.builder.Builder.While;

public class UIBuilder {
	
	private String greeting = "Hello!\n";
	private String prompt = "Enter command: ";
	private String farewell = "\nGood bye!\n";
	private String unbalanced = "There are unbalanced brackets!\n";
	private String invalidOp = "Invalid operator: ";
	
	// base pointer offsets
	private int bpBuffer = 0;
	private int bpCurrent = 1;
	private int bpNumber = 2;
	private int bpOpenBrackets = 3;
	private int bpInvalidOperator = 4;
	
	public String getUI() {
		String program = "[]0 0 0 0 ";
		program += Builder.WriteString(greeting);
		program += inputLoop(5);
		program += Builder.WriteString(farewell);
		
		program = Builder.trim(program);
		return program;
	}
	
	private String inputLoop(int bp) {
		String p = Builder.WriteString(prompt);
		p += While("r1", inputLoopBody(bp+2));
		return p;
	}
	
	private String inputLoopBody(int bp) {
		// 1 is current character
		String p = "1c";
		p += If(Builder.checkIfDigit(), readDigit(bp), nonDigitRead(bp));
		return p;
	}
	
	private String readDigit(int bp) {
		String p = "";
		p += Builder.parseDigit();
		p += Builder.MoveToTop(bp-bpCurrent);
		p += "10*+"; // append digits
		bp--;

		p += (bp-bpNumber+1) + "d1";
		p += Builder.MoveDown(2, bp-bpNumber);
		
		return p;
	}
	
	private String nonDigitRead(int bp) {
		String p = "";
		
		// if we were writing a number before, push it into the buffer
		p += If((bp-bpNumber) + "c", pushNumber(bp), "");
		
		
		String equalsBracketOpen = (int)'[' + "=";
		String equalsBracketClose = (int)']' + "=";
		String equalsSpace = (int)' ' + "=";
		String equalsNewline = (int)'\n' + "=";
		String equalsEOF = "1~=";
		
		p += If("1c" + equalsBracketOpen, pushNewBlock(bp),
				If("1c" + equalsBracketClose, pushBlock(bp),
					If("1c" + equalsNewline, execute(bp) + printPrompt(), 
						If("1c" + equalsEOF, execute(bp) + exit(), 
							If("1c" + equalsSpace, "", 
								pushOperator(bp)
							)
						)
					))
				);
				
		p += "1d"; // remove char
		return p;
	}
	
	private String printPrompt() {
		return Builder.WriteString(prompt);
	}
	
	private String pushNumber(int bp) {
		String p = "";
		p += Builder.MoveToTop(bp-bpBuffer); // buffer
		p += Builder.MoveToTop(bp-bpBuffer); // number
		p += "g"; // group
		p += (bp-bpNumber+1) + "d";
		p += "0 0 "; // new current & number
		p += Builder.MoveDown(3, bp-bpBuffer-2); // move buffer & number down
		return p;
	}
	
	private String pushNewBlock(int bp) {
		String p = "";
		
		// increase open brackets count
		p += Builder.MoveToTop(bp-bpOpenBrackets);
		p += "1+";
		
		p += If("1c0>", "1-~", ""); // if there are negative open brackets, don't ever become positive again
		
		p += Builder.MoveDown(bp-bpOpenBrackets);
		
		p += "[]";
		p += Builder.MoveDown(bp-bpBuffer);
		return p;
	}
	
	private String pushBlock(int bp) {
		String p = "";
		
		// decrease open brackets count
		p += Builder.MoveToTop(bp-bpOpenBrackets);
		p += "1-~";
		
		// if there are negative open brackets, don't ever become positive again
		// also, push a fake block so that we don't crash
		p += If("1c0>", "1-~"
			+ "[]" + Builder.MoveDown(bp-bpBuffer+1), ""); 
		p += Builder.MoveDown(bp-bpOpenBrackets);
		
		p += Builder.MoveToTop(bp-bpBuffer+1);
		p += Builder.MoveToTop(bp-bpBuffer+1);
		p += "bg"; // merge!
		p += Builder.MoveDown(bp-bpBuffer);
		return p;
	}
	
	private String pushOperator(int bp) {
		String p = "";
		
		int[] ops = new int[] {
			'a', '+', 'b', 'c', 'd', '/', '=', 'x', '>', '<', '&', '|', '%', '*', '~', 'r', '-', 'w'
		};
		
		p += "1c";
		p += If(Builder.equalsAnyOperator(ops),
			"1cb" // build operator
			+ Builder.MoveToTop(bp-bpBuffer+1)
			+ Builder.MoveToTop(2)
			+ "g"
			+ Builder.MoveDown(bp-bpBuffer)
			
			// store invalid operator
			, (bp-bpInvalidOperator)+"d"
			+ "1c"
			+ Builder.MoveDown(bp-bpInvalidOperator));
		
		return p;
	}
	
	private String execute(int bp) {
		String p = "";
		
		String exec = "";
		
		// magic values, so that we can clean up leftover state from the executed command
		int[] magicValues = new int[] { 0x1dea, 0xdead, 0xbeef };
		for (int value : magicValues) {
			exec += " " + value;
		}
		
		// move buffer to top
		exec += Builder.MoveToTop(bp-bpBuffer+3);
		
		// clear buffer
		exec += "[]";
		exec += Builder.MoveDown(bp-bpBuffer+4);
		
		exec += "a"; // apply it!
		
		exec += Builder.deleteUntilOperator(magicValues);
		for (int i = 0; i < magicValues.length; i++) {
			exec += "1d";
		}
		
		exec += "10w"; // newline
		
		// safety checks
		p += " 1 "; // the everything-good variable
		bp++;
		
		// check if brackets are balanced
		p += If((bp-bpOpenBrackets)+"c0=", "", "0&" + rebalance(bp));
		
		// check if invalid operator was used
		p += If((bp-bpInvalidOperator)+"c0=", "", "0&" + resetInvalidOperator(bp));
		bp--;
		p += If("", exec, resetBuffer(bp));
		
		return p;
	}
	
	private String rebalance(int bp) {
		String p = "";
		
		p += Builder.WriteString(unbalanced);
		p += (bp-bpOpenBrackets) + "c";
		p += If("0>", "",
				// clear multiple open buffers
				  (bp-bpOpenBrackets) + "c"
				+ While("2c0<", 
					  (bp-bpBuffer+2) + "d"
					+ "2c1-~3d"
					+ Builder.MoveDown(2)
					)
				+ "1d");
		
		// reset open bracket count
		p += (bp-bpOpenBrackets) + "d";
		p += "0";
		p += Builder.MoveDown(bp-bpOpenBrackets);
		
		return p;
	}
	
	private String resetInvalidOperator(int bp) {
		String p = "";
		
		p += Builder.MoveToTop(bp-bpInvalidOperator);
		if (!invalidOp.isEmpty()) {
			p += Builder.WriteString("Invalid operator: ");
			p += "w";
			p += "10w";
		} else {
			p += "1d";
		}
		
		p += "0";
		p += Builder.MoveDown(bp-bpInvalidOperator);
		
		return p;
	}
	
	private String resetBuffer(int bp) {
		String p = "";
		
		p += Builder.MoveToTop(bp-bpBuffer);
		p += "1d[]";
		p += Builder.MoveDown(bp-bpBuffer);
		
		return p;
	}
	
	private String exit() {
		String p = "";
		p += Builder.WriteString(farewell);
		p += "x";
		return p;
	}
	
	
	public void silence() {
		this.greeting = "";
		this.prompt = "";
		this.farewell = "";
		this.unbalanced = "";
		this.invalidOp = "";
	}


	public void setGreeting(String greeting) {
		this.greeting = greeting;
	}


	public void setPrompt(String prompt) {
		this.prompt = prompt;
	}


	public void setFarewell(String farewell) {
		this.farewell = farewell;
	}
	
	public static void main(String[] args) {
	    UIBuilder ui = new UIBuilder();
	    System.out.println(ui.getUI());
    }
}
