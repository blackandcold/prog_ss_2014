package ps14.calculator.elements;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import ps14.calculator.Context;

public class BlockElement implements IElement {
	private List<IElement> elements = new ArrayList<>();
	
	@Override
	public void apply(Context ctxt) {
		ctxt.getDataStack().push(this);
	}
	
	public List<IElement> getElements() {
		return this.elements;
	}
	
	@Override
	public String toString() {
		String inner = elements.stream().map(e->e.toString()).collect(Collectors.joining(" "));
	    return "[" + inner + "]";
	}

}
