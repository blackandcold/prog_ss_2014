package ps14.calculator.elements;

import ps14.calculator.Context;


public class IntegerElement implements IElement {
	int value;
	
	public IntegerElement(int value) {
	    this.value = value;
    }
	
	public int getValue() {
		return value;
	}
	
	@Override
	public void apply(Context ctxt) {
	    ctxt.getDataStack().push(this);
	}
	
	@Override
	public String toString() {
	    return String.valueOf(value);
	}
}
