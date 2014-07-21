package ps14.calculator.elements.operators;

import ps14.calculator.Context;
import ps14.calculator.elements.IElement;


public class LessThanOperator implements IElement {
	public static final char OPERATOR = '<';
	
	@Override
	public void apply(Context ctxt) {
	    // TODO take 2 ints from stack, compare them   
	}
	
	@Override
	public String toString() {
		return Character.toString(OPERATOR);
	}
}
