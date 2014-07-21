package ps14.calculator.elements;

import ps14.calculator.Context;

/**
 * A data element in the calculator.
 * Might be an integer, an operator, or a block
 */
public interface IElement {
	public void apply(Context ctxt);
}
