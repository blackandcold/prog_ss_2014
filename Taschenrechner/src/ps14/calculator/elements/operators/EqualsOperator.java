package ps14.calculator.elements.operators;

import ps14.calculator.CalculatorException;
import ps14.calculator.Context;
import ps14.calculator.elements.IElement;
import ps14.calculator.elements.IntegerElement;

/**
 * Compares two elements, pushes 1 onto the data stack if they are equal, 0 otherwise.
 */
public class EqualsOperator implements IElement {
	public static final char OPERATOR = '=';
	
	@Override
	public void apply(Context ctxt) {
		if (ctxt.getDataStack().size() < 2) {
			throw new CalculatorException("Need 2 arguments on data stack to compare");
		}
		
		IElement arg1 = ctxt.getDataStack().pop();
		IElement arg2 = ctxt.getDataStack().pop();
		
		boolean equal = arg1.equals(arg2);
	    ctxt.getDataStack().push(new IntegerElement(equal ? 1 : 0));
	}
	
	@Override
	public String toString() {
		return Character.toString(OPERATOR);
	}
}
