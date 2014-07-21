package ps14.calculator.elements.operators;

import ps14.calculator.Context;
import ps14.calculator.elements.IElement;
import ps14.calculator.elements.IntegerElement;

/**
 * Takes two integers from the data stack and compares them.
 */
public class GreaterThanOperator implements IElement {
	public static final char OPERATOR = '>';
	
	@Override
	public void apply(Context ctxt) {
		IntegerElement i1 = (IntegerElement) ctxt.getDataStack().pop();
		IntegerElement i2 = (IntegerElement) ctxt.getDataStack().pop();
		
		boolean greater = i1.getValue() > i2.getValue();
		ctxt.getDataStack().push(new IntegerElement(greater ? 1 : 0));   
	}
	
	@Override
	public String toString() {
		return Character.toString(OPERATOR);
	}
}
