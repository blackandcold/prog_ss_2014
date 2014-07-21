package ps14.calculator.elements.operators;

import ps14.calculator.Context;
import ps14.calculator.elements.IElement;
import ps14.calculator.elements.IntegerElement;

/**
 * Takes two integers from the data stack and adds them.
 */
public class AdditionOperator implements IElement {
	public static final char OPERATOR = '+';
	
	@Override
	public void apply(Context ctxt) {
		IntegerElement i1 = (IntegerElement) ctxt.getDataStack().pop();
		IntegerElement i2 = (IntegerElement) ctxt.getDataStack().pop();
		
		ctxt.getDataStack().push(new IntegerElement(i1.getValue() + i2.getValue()));
	}
	
	@Override
	public String toString() {
		return Character.toString(OPERATOR);
	}
}
