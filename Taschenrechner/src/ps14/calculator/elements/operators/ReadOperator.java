package ps14.calculator.elements.operators;

import ps14.calculator.Context;
import ps14.calculator.elements.IElement;
import ps14.calculator.elements.IntegerElement;

/**
 * Takes the next character code from the input stream and pushes it onto the data stack.
 */
public class ReadOperator implements IElement {
	public static final char OPERATOR = 'r';
	
	@Override
	public void apply(Context ctxt) {
		int i = ctxt.getInputStream().read();
		ctxt.getDataStack().push(new IntegerElement(i));
	}
	
	@Override
	public String toString() {
		return Character.toString(OPERATOR);
	}
}
