package ps14.calculator.elements.operators;

import ps14.calculator.Context;
import ps14.calculator.elements.IElement;


public class BuildBlockOperator implements IElement {
	public static final char OPERATOR = 'b';
	
	@Override
	public void apply(Context ctxt) {
	    // TODO
		// takes an argument from the data stack and creates a new block.
		// If the argument is a block, then a new block containing just this block is created.
		// If the argument is an integer corresponding to the ASCII code of a character representing an operation,
		// then the result is a block containing just this operation. Otherwise an error is reported.
	}
	
	@Override
	public String toString() {
		return Character.toString(OPERATOR);
	}
}
