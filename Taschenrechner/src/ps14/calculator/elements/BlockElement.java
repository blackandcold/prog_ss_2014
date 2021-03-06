package ps14.calculator.elements;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

import ps14.calculator.Context;

public class BlockElement implements IElement {
	private List<IElement> elements = new ArrayList<>();
	
	public BlockElement() {
    }
	
	public BlockElement(Collection<IElement> elements) {
		this.elements.addAll(elements);
	}
	
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
	
	@Override
	public boolean equals(Object obj) {
	    if (obj instanceof BlockElement) {
	    	BlockElement other = (BlockElement) obj;
	    	if (elements.size() != other.getElements().size())
	    		return false;
	    	
	    	for (int i = 0; i < elements.size(); i++) {
	    		boolean equal = this.elements.get(i).equals(other.elements.get(i));
	    		if (!equal) return false;
	    	}
	    	
	    	return true;
	    }
	    return false;
	}

}
