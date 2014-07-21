package ps14.calculator.elements.operators;

import ps14.calculator.Context;
import ps14.calculator.elements.IElement;


public class ApplyOperator implements IElement {
	public static final char OPERATOR = 'a';
	
	@Override
	public void apply(Context ctxt) {
	    // TODO take block from data stack and apply it   
	}
	
	@Override
	public String toString() {
		return Character.toString(OPERATOR);
	}
}
