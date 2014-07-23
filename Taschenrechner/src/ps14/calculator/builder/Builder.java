package ps14.calculator.builder;

/**
 * Convenience functions to build programs
 * in the language of the calculator.
 */
public class Builder {
	
	/**
	 * Builds an if-construct.
	 * @param condition Condition to check
	 * @param then Code to execute if condition is 1
	 * @param otherwise Code to execute if condition is 0
	 */
	public static String If(String condition, String then, String otherwise) {
		return String.format("%s[%s][%s][3c4d1+da]a", condition, otherwise, then);
	}
	
	/**
	 * Builds a while loop. 
	 * @param condition as long the top element of the stack after executing the condition is 1 the body is executed
	 * @param body the loop body
	 */
	public static String While(String condition, String body) {
		String check = If(condition, body + "1ca", "1d");
		return "["+check+"]1ca";
	}
	
	/**
	 * Writes the string as ASCII using the write operator.
	 * @param Str string to write
	 */
	public static String WriteString(String Str) {
		String result = "";
		for (char ch : Str.toCharArray()) {
			result += (int)ch + "w";
		}
		return result;
	}
	
	/**
	 * Applied to a binary, returns the negated result.
	 * Otherwise, everything other than 0 is interpreted as true.
	 */
	public static String Not(String condition) {
		return condition + "0=";
	}
	
	/**
	 * Moves the item at the given position (from the top of the stack) to the top
	 * @param position of stack item to move up
	 */
	public static String MoveToTop(int position) {
		return "["+position + "c" + (position+1) + "d]a";
	}
	
	/**
	 * Moves the item at the top of the stack (pos 1) down to the given position
	 * @param position of stack item to move down
	 */
	public static String MoveDown(int position) {
		String result = "";
		for (int i = 0; i < position - 1; i++) {
			result += MoveToTop(position);
		}
		return "["+result+"]a";
	}
	
	/**
	 * Takes the top element of the stack and replaces it
	 * with 1 if it is the ASCII code for a letter
	 * or 0 otherwise
	 */
	public static String checkIfLetter() {
		// ASCII code in the range A-Z (65-90) or a-z (97-122)
		return "[1c 64< 2c 91> & 2c 96< 3c 123> & | 2d]a";
	}
	
	/**
	 * Takes the top element of the stack and replaces it
	 * with 1 if it is the ASCII code for a digit
	 * or 0 otherwise
	 */
	public static String checkIfDigit() {
		// ASCII code in the range 0-9 (48-57)
		return "[48-~1c1~<2c10>&2d]a";
	}
	
	/**
	 * Applied to a positive int, puts the digit count onto the stack
	 */
	public static String digitCountOperator() {
		String p = "";
		
		p += If("1c0>", "1~*", "");
		p += "1"; // starting count at 1
		p += While(
				  "3c" // number 
				+ "3c" // count
				+ "10" + powerOperator()
				+ "1-~<"
				
				, MoveToTop(2)
				+ "1+"
				+ MoveDown(2));
		p += "2d";
		return "[" + p + "]a";
	}
	
	/**
	 * Writes the int on the top of the stack as ASCII
	 * using the write operator
	 */
	public static String WriteInt() {
		String p = "";
		
		p += If("1c0>", "45w", ""); // minus symbol
		
		p += "1c"; // copy
		p += digitCountOperator();
		
		// 1 is while
		// 2 is digit count
		// 3 is the number
		p += While("2c0<",
				  "3c" // copy the number
				+ mostSignificantDigitOperator()
				+ "48+w" // convert to ascii and write
				+ MoveToTop(2)
				+ "1-~" // decrease digit count
				+ MoveDown(2)
				+ "2c 10" + powerOperator() // 10 ^ digit count
				+ MoveToTop(3+1)
				+ "%"
				+ MoveDown(3)
				);
		p += "1d1d";
		
		return "[" + p + "]a";
	}
	
	/**
	 * Replaces the int on top of the stack with its most significant digit
	 */
	public static String mostSignificantDigitOperator() {
		String p = "";
		
		p += "1c";
		p += digitCountOperator();
		p += "1-~";
		p += "10";
		p += powerOperator();
		p += MoveToTop(2);
		p += "/";
		
		p = If("1c10>", "", p);
		p = If("1c0>", "1~*"+p, p);
		
		return "[" + p + "]a";
	}
	
	/**
	 * Takes 2 ints from stack, puts int1 ^ int2 onto data stack
	 */
	public static String powerOperator() {
		// 1 is base
		// 2 is exponent
		String exp = "";
		
		exp += "1c"; // copy base
		exp += While("4c1<",
				MoveToTop(4)
			  + "1-~" // decrease exponent
			  + MoveDown(4) // move back exponent
			  + MoveToTop(3) // get num
			  + "3c" // copy base
			  + "*"
			  + MoveDown(3));
		exp += "1d2d"; // cleanup
		
		String p = "";
		p += If("2c1>", "1d1d1", exp);
		return "[" + p + "]a";
	}
	
	/**
	 * Replaces the ASCII digit on top of the stack with the corresponding integer
	 * @return
	 */
	public static String parseDigit() {
		return "[48-~]a";
	}
	
	/**
	 * Reads from the input stream as long as there are digits available
	 * and puts the read number onto the data stack
	 */
	public static String readNumber() {
		String p = "0";
		p += While("r1c3c4d2c3d" + checkIfDigit(), "2c3d" + parseDigit() + "3c4d10*+2c3d");
		p += "1d";
		return "["+p+"]a";
	}
}
