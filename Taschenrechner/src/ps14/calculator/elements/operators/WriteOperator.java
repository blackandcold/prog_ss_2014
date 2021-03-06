package ps14.calculator.elements.operators;

import ps14.calculator.CalculatorException;
import ps14.calculator.Context;
import ps14.calculator.elements.IElement;

/**
 * Takes an integer from the top of the data stack and writes it as character code into the output stream.
 * An error is reported if the argument is not a number in the range of character codes.
 */
public class WriteOperator implements IElement {
	public static final char OPERATOR = 'w';
	
	@Override
	public void apply(Context ctxt) {
		int charCode = ctxt.nextInt().getValue();
		
		if (charCode > 0x7E || (charCode < 0x20 && charCode != 0x7 && charCode != 0x8 && charCode != 0xA && charCode != 0xD))
			throw new CalculatorException("ASCII code " + charCode + " is not in the range of printable characters!");
		
		ctxt.getOutputStream().write(charCode);
	}
	
	@Override
	public String toString() {
		return Character.toString(OPERATOR);
	}
}
