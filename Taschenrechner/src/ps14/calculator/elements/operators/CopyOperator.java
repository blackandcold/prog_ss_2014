package ps14.calculator.elements.operators;

import ps14.calculator.Context;
import ps14.calculator.elements.IElement;


public class CopyOperator implements IElement {
	public static final char OPERATOR = 'c';
	
	@Override
	public void apply(Context ctxt) {
	    // TODO
		// replaces the top element n of the data stack with a copy of the nth element on the data stack (counted from the top of the stack).
		// An error is reported if n is not a positive number or the stack does not contain a sufficient number of elements.
	}
	
	@Override
	public String toString() {
		return Character.toString(OPERATOR);
	}
}
