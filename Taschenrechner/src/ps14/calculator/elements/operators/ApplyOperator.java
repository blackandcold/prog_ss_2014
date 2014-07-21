package ps14.calculator.elements.operators;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import ps14.calculator.CalculatorException;
import ps14.calculator.Context;
import ps14.calculator.elements.BlockElement;
import ps14.calculator.elements.IElement;
import ps14.calculator.elements.IntegerElement;

/**
 * If the single argument is a block, then the block is taken from the data stack and
 * its content is pushed onto the code stack so that the operations in the block are executed next.
 * Otherwise the operation has no effect.
 */
public class ApplyOperator implements IElement {
	public static final char OPERATOR = 'a';
	
	@Override
	public void apply(Context ctxt) {
		IElement el = ctxt.getDataStack().pop();
		
		if (el instanceof IntegerElement) {
			ctxt.getDataStack().push(el);
		} else if (el instanceof BlockElement) {
		    BlockElement block = (BlockElement) el;
		    List<IElement> elements = new ArrayList<>(block.getElements());
		    Collections.reverse(elements);
		    ctxt.getCodeStack().addAll(elements);
		} else {
			throw new CalculatorException("Argument cannot be applied.");
		}
	}
	
	@Override
	public String toString() {
		return Character.toString(OPERATOR);
	}
}
