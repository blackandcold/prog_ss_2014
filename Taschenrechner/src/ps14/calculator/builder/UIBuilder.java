package ps14.calculator.builder;

import static ps14.calculator.builder.Builder.If;
import static ps14.calculator.builder.Builder.While;

public class UIBuilder {
	
	private String greeting = "Hello!\n";
	private String prompt = "Enter command: ";
	private String farewell = "\nGood bye!\n";
	
	private int bpBuffer = 0;
	private int bpCurrent = 1;
	private int bpNumber = 2;
	
	public String getUI() {
		String program = "[]0 0 ";
		program += Builder.WriteString(greeting);
		program += inputLoop(3);
		program += Builder.WriteString(farewell);
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
		p += Builder.MoveDown(bp-bpCurrent);
		p += (bp-bpNumber) + "d1";
		p += Builder.MoveDown(bp-bpNumber);
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
		p += "0"; // new number
		p += Builder.MoveDown(bp-bpBuffer); // move number down
		p += Builder.MoveDown(bp-bpBuffer); // move buffer down
		p += (bp-bpNumber) + "d";
		p += "0";
		p += Builder.MoveDown(bp-bpNumber);
		return p;
	}
	
	private String pushNewBlock(int bp) {
		String p = "";
		p += "[]";
		p += Builder.MoveDown(bp-bpBuffer);
		return p;
	}
	
	private String pushBlock(int bp) {
		String p = "";
		p += Builder.MoveToTop(bp-bpBuffer+1);
		p += Builder.MoveToTop(bp-bpBuffer+1);
		p += "bg"; // merge!
		p += Builder.MoveDown(bp-bpBuffer);
		return p;
	}
	
	private String pushOperator(int bp) {
		String p = "";
		p += "1cb"; // build operator
		p += Builder.MoveToTop(bp-bpBuffer+1);
		p += Builder.MoveToTop(2);
		p += "g";
		p += Builder.MoveDown(bp-bpBuffer);
		return p;
	}
	
	private String execute(int bp) {
		String p = "";
		p += Builder.MoveToTop(bp-bpBuffer);
		p += "[]";
		p += Builder.MoveDown(bp-bpBuffer+1); // clear buffer
		p += "a"; // apply it!
		
		p += "10w"; // newline
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
