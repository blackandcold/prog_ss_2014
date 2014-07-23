package ps14.calculator;

import java.util.ArrayList;
import java.util.List;
import java.util.Stack;
import java.util.stream.Collectors;

import ps14.calculator.elements.BlockElement;
import ps14.calculator.elements.IElement;
import ps14.calculator.elements.IntegerElement;
import ps14.calculator.streams.Display;
import ps14.calculator.streams.IStream;
import ps14.calculator.streams.InStream;

public class Context {
	private Stack<IElement> dataStack, codeStack;
	private IStream inStream, outStream;
	
	public Context() {
		this(new Stack<IElement>(), new Stack<IElement>());
	}
	
	public Context(Stack<IElement> dataStack, Stack<IElement> codeStack) {
		this.dataStack = dataStack;
		this.codeStack = codeStack;
		
		inStream = new InStream();
		outStream = new Display();
	}
	
	public IStream getInputStream() {
		return inStream;
	}
	
	public IStream getOutputStream() {
		return outStream;
	}
	
	public void setInputStream(IStream inStream) {
		this.inStream = inStream;
	}
	
	public void setOutputStream(IStream outStream) {
		this.outStream = outStream;
	}
	
	public Stack<IElement> getDataStack() {
		return dataStack;
	}
	
	public Stack<IElement> getCodeStack() {
		return codeStack;
	}
	
	
	public IntegerElement nextInt() {
		if (dataStack.size() > 0) {
			IElement element = dataStack.peek();
			if (element instanceof IntegerElement)
				return (IntegerElement) dataStack.pop();
		}
		
		throw new CalculatorException("Expected Integer on data stack");
	}
	
	public BlockElement nextBlock() {
		if (dataStack.size() > 0) {
			IElement element = dataStack.peek();
			if (element instanceof BlockElement)
				return (BlockElement) dataStack.pop();
		}
		
		throw new CalculatorException("Expected Block on data stack");
	}
	
	
	
	@Override
	public String toString() {
		List<String> result = new ArrayList<>();
		for (IElement e : dataStack) {
			result.add(e.toString());
		}
		result.add("^");
		int insertionPoint = result.size();
		for (IElement e : codeStack) {
			result.add(insertionPoint, e.toString());
		}
		
		return result.stream().collect(Collectors.joining(" "));
	}
}
