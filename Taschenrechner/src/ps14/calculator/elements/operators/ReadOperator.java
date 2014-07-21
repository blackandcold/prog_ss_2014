package ps14.calculator.elements.operators;

import ps14.calculator.Context;
import ps14.calculator.elements.IElement;


public class ReadOperator implements IElement {
	public static final char OPERATOR = 'r';
	
	@Override
	public void apply(Context ctxt) {
	    // TODO
		// takes the next character code from the input stream and pushes it onto the data stack.
	}
	
	@Override
	public String toString() {
		return Character.toString(OPERATOR);
	}
}
