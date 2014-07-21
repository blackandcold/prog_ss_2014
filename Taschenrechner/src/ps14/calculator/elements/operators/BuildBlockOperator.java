package ps14.calculator.elements.operators;

import java.util.ArrayList;
import java.util.List;

import ps14.calculator.CalculatorException;
import ps14.calculator.Context;
import ps14.calculator.elements.BlockElement;
import ps14.calculator.elements.IElement;
import ps14.calculator.elements.IntegerElement;

/**
 * Takes an argument from the data stack and creates a new block.
 * If the argument is a block, then a new block containing just this block is created.
 * If the argument is an integer corresponding to the ASCII code of a character representing an operation,
 * then the result is a block containing just this operation.
 * Otherwise an error is reported.
 */
public class BuildBlockOperator implements IElement {
	public static final char OPERATOR = 'b';
	
	@Override
	public void apply(Context ctxt) {
	    IElement arg = ctxt.getDataStack().pop();
	    
	    List<IElement> elements = new ArrayList<>();
	    if (arg instanceof BlockElement) {
	    	elements.add(arg);
	    } else if (arg instanceof IntegerElement) {
	    	char opCode = (char) ((IntegerElement) arg).getValue();
	    	IElement op = OperatorFactory.getOperator(opCode);
	    	
	    	if (op == null)
	    		throw new CalculatorException("Could not build block form operator char code: Unknown operator '" + opCode + "'");
	    	
	    	elements.add(op);
	    }
	    
	    ctxt.getDataStack().push(new BlockElement(elements));
	}
	
	@Override
	public String toString() {
		return Character.toString(OPERATOR);
	}
}
