package ps14.calculator.elements.operators;

import ps14.calculator.Context;
import ps14.calculator.elements.IElement;


public class DeleteOperator implements IElement {
	public static final char OPERATOR = 'd';
	
	@Override
	public void apply(Context ctxt) {
	    // TODO
		// takes the top element n from the data stack and additionally removes the nth element from the data stack (counted from the top of the stack).
		// An error is reported if n is not a positive number or the stack does not contain a sufficient number of elements.

	}
	
	@Override
	public String toString() {
		return Character.toString(OPERATOR);
	}
}
