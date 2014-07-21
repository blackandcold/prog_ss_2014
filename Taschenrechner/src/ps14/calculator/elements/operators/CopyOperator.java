package ps14.calculator.elements.operators;

import ps14.calculator.CalculatorException;
import ps14.calculator.Context;
import ps14.calculator.elements.BlockElement;
import ps14.calculator.elements.IElement;

/**
 * Replaces the top element n of the data stack with a copy of the nth element on the data stack (counted from the top of the stack).
 * An error is reported if n is not a positive number or the stack does not contain a sufficient number of elements.
 */
public class CopyOperator implements IElement {
	public static final char OPERATOR = 'c';
	
	@Override
	public void apply(Context ctxt) {
	    int arg = ctxt.nextInt().getValue();
	    
	    if (arg < 1)
	    	throw new CalculatorException("Copy index must be positive!");
	    
	    if (arg > ctxt.getDataStack().size())
	    	throw new CalculatorException("Copy index out of range!");
	    
	    int index = ctxt.getDataStack().size() - arg;
	    IElement el = ctxt.getDataStack().get(index);
	    
	    if (el instanceof BlockElement) {
	    	ctxt.getDataStack().push(new BlockElement(((BlockElement) el).getElements()));
	    } else {
	    	ctxt.getDataStack().push(el);
	    }
	}
	
	@Override
	public String toString() {
		return Character.toString(OPERATOR);
	}
}
