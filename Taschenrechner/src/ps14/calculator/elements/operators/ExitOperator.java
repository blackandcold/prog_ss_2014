package ps14.calculator.elements.operators;

import ps14.calculator.Context;
import ps14.calculator.elements.IElement;


public class ExitOperator implements IElement {
	public static final char OPERATOR = 'x';
	
	@Override
	public void apply(Context ctxt) {
	    // TODO
		// stops the execution of the calculator.
	}
	
	@Override
	public String toString() {
		return Character.toString(OPERATOR);
	}
}
