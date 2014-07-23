package ps14.calculator.parser;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import ps14.calculator.Context;
import ps14.calculator.elements.BlockElement;
import ps14.calculator.elements.IElement;
import ps14.calculator.elements.IntegerElement;
import ps14.calculator.elements.operators.OperatorFactory;

/**
 * Creates a Calculator Context from a String
 * by parsing the language of the calculator.
 */
public class Parser {
	private String input;
	
	public Context parse(String input) throws ParseException {
		this.input = input;
		Context context = new Context();
		
		if (mismatchedBrackets()) {
			throw new MismatchedBracketsException();
		}
		
		String[] parts = input.split("\\^");
		if (parts.length > 2) {
			throw new ParseException("Too many stack segments!");
		}
		
		List<IElement> elements;
		
		if (parts.length == 2) {
			input = parts[0];
			elements = parseElements(input);
			context.getDataStack().addAll(elements);
			
			input = parts[1];
		} else {
			input = parts[0];
		}
		
		elements = parseElements(input);
		Collections.reverse(elements);
		context.getCodeStack().addAll(elements);
		
		return context;
	}
	
	private List<IElement> parseElements(String input) throws ParseException {
		this.input = input;
		List<IElement> elements = new ArrayList<>();
		IElement element;
		
		while ((element = nextElement()) != null) {
			elements.add(element);
		}
		
		return elements;
	}
	
	private IElement nextElement() throws ParseException {
		// remove whitespace
		input = input.trim();
		
		if (input.isEmpty())
			return null;
		
		char ch = input.charAt(0);
		if (Character.isDigit(ch)) {
			return nextInt();
		} else if (ch == '[') {
			return nextBlock();
		} else {
			return nextOperator();
		}
	}
	
	private IntegerElement nextInt() {
		int val = 0;
		while (!input.isEmpty() && Character.isDigit(input.charAt(0))) {
			val *= 10;
			val += Character.getNumericValue(input.charAt(0));
			input = input.substring(1);
		}
		
		return new IntegerElement(val);
	}
	
	private IElement nextOperator() throws ParseException {
		char ch = input.charAt(0);
		input = input.substring(1);
		IElement operator = OperatorFactory.getOperator(ch);
		
		if (operator == null) {
			throw new ParseException("Unknown operator: " + ch);
		}
		
		return operator;
	}
	
	private BlockElement nextBlock() throws ParseException {
		// find matching bracket
		int openBrackets = 1;
		int i;
		for (i = 1; i < input.length(); i++) {
			if (input.charAt(i) == '[') {
				openBrackets++;
			} else if (input.charAt(i) == ']') {
				openBrackets--;
			}
			if (openBrackets == 0) break;
		}
		
		// parse block content
		String inner = input.substring(1, i);
		BlockElement block = new BlockElement();
		
		Parser p = new Parser();
		block.getElements().addAll(p.parseElements(inner));
		
		input = input.substring(i+1);
		return block;
	}
	
	
	/**
	 * Check for mismatched brackets
	 * @return <tt>true</tt> if there are mismatched brackets,
	 *         <tt>false</tt> otherwise
	 */
	private boolean mismatchedBrackets() {
		int openBrackets = 0;
		for (int i = 0; i < input.length(); i++) {
			if (input.charAt(i) == '[') {
				openBrackets++;
			} else if (input.charAt(i) == ']') {
				openBrackets--;
			}
			if (openBrackets < 0) return true;
		}
		
		return openBrackets > 0;
	}
	
	
}
