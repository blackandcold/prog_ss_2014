package ps14.calculator.elements.operators;

import ps14.calculator.CalculatorException;
import ps14.calculator.Context;
import ps14.calculator.elements.IElement;
import ps14.calculator.elements.IntegerElement;

/**
 * Takes two integers from the data stack and applies logical OR to them.
 * If the integers are not binary (0 or 1) an error is reported.
 */
public class LogicalOrOperator implements IElement {
	public static final char OPERATOR = '|';
	
	@Override
	public void apply(Context ctxt) {
		int i1 = ctxt.nextInt().getValue();
		int i2 = ctxt.nextInt().getValue();
		
		if (i1 < 0 || i1 > 1 || i2 < 0 || i2 > 1)
			throw new CalculatorException("Arguments for logical or have to be binary!");
		
		ctxt.getDataStack().push(new IntegerElement(i1 | i2));
	}
	
	@Override
	public String toString() {
		return Character.toString(OPERATOR);
	}
}
