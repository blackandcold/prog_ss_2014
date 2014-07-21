package ps14.calculator.elements.operators;

import ps14.calculator.Context;
import ps14.calculator.elements.IElement;


public class NegationOperator implements IElement {
	public static final char OPERATOR = '~';
	
	@Override
	public void apply(Context ctxt) {
	    // TODO take an int from the stack, negate it   
	}
	
	@Override
	public String toString() {
		return Character.toString(OPERATOR);
	}
}
