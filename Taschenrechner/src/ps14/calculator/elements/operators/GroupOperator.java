package ps14.calculator.elements.operators;

import java.util.ArrayList;
import java.util.List;

import ps14.calculator.CalculatorException;
import ps14.calculator.Context;
import ps14.calculator.elements.BlockElement;
import ps14.calculator.elements.IElement;
import ps14.calculator.elements.IntegerElement;

/**
 * Takes two arguments from the data stack and composes a new block from them. 
 * If both arguments are blocks, then their contents are simply appended.
 * If one argument is a block and the other an integer, then the integer is inserted into the block.
 * If both arguments are integers, then a new block consisting of these integers is constructed.
 */
public class GroupOperator implements IElement {
	public static final char OPERATOR = 'g';
	
	@Override
	public void apply(Context ctxt) {		
		IElement arg2 = ctxt.getDataStack().pop();
		IElement arg1 = ctxt.getDataStack().pop();
		
		List<IElement> elements = new ArrayList<>();
		if (arg1 instanceof BlockElement) {
			BlockElement block1 = (BlockElement) arg1;
			elements.addAll(block1.getElements());
		} else if (arg1 instanceof IntegerElement) {
			IntegerElement int1 = (IntegerElement) arg1;
			elements.add(int1);
		} else {
			throw new CalculatorException("The group operator expects either a block or an integer as arguments");
		}
		
		if (arg2 instanceof BlockElement) {
			BlockElement block2 = (BlockElement) arg2;
			elements.addAll(block2.getElements());
		} else if (arg2 instanceof IntegerElement) {
			IntegerElement int2 = (IntegerElement) arg2;
			elements.add(int2);
		} else {
			throw new CalculatorException("The group operator expects either a block or an integer as arguments");
		}
		
		ctxt.getDataStack().push(new BlockElement(elements));
	}
	
	@Override
	public String toString() {
		return Character.toString(OPERATOR);
	}
}
