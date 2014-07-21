package ps14.calculator.elements.operators;

import ps14.calculator.Context;
import ps14.calculator.elements.IElement;
import ps14.calculator.elements.IntegerElement;

/**
 * Compares two integers, pushes 1 onto the data stack if they are equal, 0 otherwise.
 */
public class EqualsOperator implements IElement {
	public static final char OPERATOR = '=';
	
	@Override
	public void apply(Context ctxt) {
	    int i1 = ctxt.nextInt().getValue();
	    int i2 = ctxt.nextInt().getValue();
	    
	    ctxt.getDataStack().push(new IntegerElement(i1 == i2 ? 1 : 0));
	}
	
	@Override
	public String toString() {
		return Character.toString(OPERATOR);
	}
}
