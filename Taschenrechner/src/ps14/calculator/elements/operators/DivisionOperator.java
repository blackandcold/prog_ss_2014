package ps14.calculator.elements.operators;

import ps14.calculator.CalculatorException;
import ps14.calculator.Context;
import ps14.calculator.elements.IElement;
import ps14.calculator.elements.IntegerElement;

/**
 * Takes two integers from the data stack and divides them.
 */
public class DivisionOperator implements IElement {
	public static final char OPERATOR = '/';
	
	@Override
	public void apply(Context ctxt) {
		int i1 = ctxt.nextInt().getValue();
		int i2 = ctxt.nextInt().getValue();
		
		if (i2 == 0)
			throw new CalculatorException("Division by zero");
		
		ctxt.getDataStack().push(new IntegerElement(i1 / i2));
	}
	
	@Override
	public String toString() {
		return Character.toString(OPERATOR);
	}
}
