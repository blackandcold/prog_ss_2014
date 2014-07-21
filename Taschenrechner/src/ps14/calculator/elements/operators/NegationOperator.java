package ps14.calculator.elements.operators;

import ps14.calculator.Context;
import ps14.calculator.elements.IElement;
import ps14.calculator.elements.IntegerElement;

/**
 * This unary operator changes the sign of an integer. An error is reported if its argument is not an integer.
 */
public class NegationOperator implements IElement {
	public static final char OPERATOR = '~';
	
	@Override
	public void apply(Context ctxt) {
	    int val = ctxt.nextInt().getValue() * -1;
	    ctxt.getDataStack().push(new IntegerElement(val));
	}
	
	@Override
	public String toString() {
		return Character.toString(OPERATOR);
	}
}
