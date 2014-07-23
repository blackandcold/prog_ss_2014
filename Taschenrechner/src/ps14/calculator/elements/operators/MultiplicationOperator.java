package ps14.calculator.elements.operators;

import ps14.calculator.Context;
import ps14.calculator.elements.IElement;
import ps14.calculator.elements.IntegerElement;

/**
 * Takes two integers from the data stack and multiplies them.
 */
public class MultiplicationOperator implements IElement {
	public static final char OPERATOR = '*';
	
	@Override
	public void apply(Context ctxt) {
		IntegerElement i1 = ctxt.nextInt();
		IntegerElement i2 = ctxt.nextInt();
		
		ctxt.getDataStack().push(new IntegerElement(i1.getValue() * i2.getValue()));
	}
	
	@Override
	public String toString() {
		return Character.toString(OPERATOR);
	}
}
