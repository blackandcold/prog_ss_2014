package ps14.calculator.builder;




public class PrimalityTestBuilder {
	
	/**
	 * An operator that replaces the int on top of the stack
	 * with 1 if it is prime, with 0 otherwise
	 */
	public String getPrimalityTestOperator() {
		String loop = "";
		loop += "0 2";
		loop += Builder.While("3c0=", 
					"2c5c%"
					+ Builder.If("0=", "2c3d3d3c=2c3d", "2c1+3d2c3d"));
		loop += "2d";
		
		String p = "";
		p += Builder.If("1c2>", "1d0", loop);
		
		return " " + p;
	}
	
	/**
	 * A prime test program that asks for a number
	 * and writes the result as text to the output stream
	 */
	public String getProgram() {
		String p = "";
		p += Builder.WriteString("Enter a number: ");
		p += Builder.readNumber();
		p += "1c";
		p += getPrimalityTestOperator();
		p += getResultStringOperator();
		
		p = Builder.trim(p);
		return p;
	}
	
	/**
	 * Prints the result as readable string.
	 * Assumes that the number and the primality-test result are on top of the stack.
	 */
	public String getResultStringOperator() {
		String p = "";
		p += Builder.MoveToTop(2);
		p += Builder.WriteInt();
		p += Builder.WriteString(" is ");
		p += Builder.If("1=", "", Builder.WriteString("not "));
		p += Builder.WriteString("a prime number\n");
		return p;
	}
	
	
	/**
	 * Output the primality test program
	 */
	public static void main(String[] args) {
	    PrimalityTestBuilder prime = new PrimalityTestBuilder();
	    System.out.println(prime.getProgram());
    }
	
}
