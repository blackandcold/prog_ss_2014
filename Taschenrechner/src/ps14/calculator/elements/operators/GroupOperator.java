package ps14.calculator.elements.operators;

import ps14.calculator.Context;
import ps14.calculator.elements.IElement;


public class GroupOperator implements IElement {
	public static final char OPERATOR = 'g';
	
	@Override
	public void apply(Context ctxt) {
	    // TODO
		// takes two arguments from the data stack and composes a new block from them. 
		// If both arguments are blocks, then their contents are simply appended.
		// If one argument is a block and the other an integer, then the integer is inserted into the block.
		// If both arguments are integers, then a new block consisting of these integers is constructed.

	}
	
	@Override
	public String toString() {
		return Character.toString(OPERATOR);
	}
}
