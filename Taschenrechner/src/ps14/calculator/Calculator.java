package ps14.calculator;

import ps14.calculator.elements.operators.ExitOperator;
import ps14.calculator.parser.ParseException;
import ps14.calculator.parser.Parser;

public class Calculator {
	private Context context;
	
	/**
	 * Creates and uses a new context from the given String.
	 * @param input The string to create a context from
	 * @return the created context
	 * @throws ParseException 
	 */
	public Context parse(String input) throws ParseException {
		Parser p = new Parser();
		this.context = p.parse(input);
		return this.context;
	}
	
	/**
	 * Creates and uses a new context from the given String
	 * and executes the program.
	 * @param input The string to create a context from
	 * @return the context after execution
	 * @throws ParseException
	 */
	public Context run(String input) throws ParseException {
		parse(input);
		return run();
	}
	
	/**
	 * Executes the program
	 * @return the context of the calculator after execution
	 */
	public Context run() {
		while (executeStep()) {};
		return context;
	}
	
	/**
	 * Executes one step of the program
	 * @return <tt>true</tt> if a step has been executed,
	 *         <tt>false</tt> otherwise
	 */
	public boolean executeStep() {
		if (context.getCodeStack().empty()) return false;
		if (context.getCodeStack().peek() instanceof ExitOperator) {
			context.getCodeStack().pop();
			return false;
		}
		
		context.getCodeStack().pop().apply(context);
		
		return true;
	}
	
	/**
	 * @return the current context of the calculator
	 */
	public Context getContext() {
		return this.context;
	}
}
