package ps14.calculator;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

import ps14.calculator.elements.operators.ExitOperator;
import ps14.calculator.parser.ParseException;
import ps14.calculator.parser.Parser;
import ps14.calculator.streams.StdOutStream;

public class Calculator {
	private Context context;
	private boolean printStateEveryStep = false;
	
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
		if (printStateEveryStep)
			System.out.println(context.toString());
		
		return true;
	}
	
	/**
	 * @return the current context of the calculator
	 */
	public Context getContext() {
		return this.context;
	}
	
	
	public static void main(String[] args) {
	    if (args.length == 0) {
	    	System.out.println("USAGE: java " + Calculator.class.getName() + " inputfile");
	    	return;
	    }
    	
	    String content;
	    try {
             content = new String(Files.readAllBytes(Paths.get(args[0])));
        } catch (IOException e) {
            System.out.println("Could not read file!");
            return;
        }
	    
	    Calculator calc = new Calculator();
	    try {
	        calc.parse(content);
	        calc.getContext().setOutputStream(new StdOutStream());
	        calc.run();
        } catch (ParseException e) {
	        e.printStackTrace();
	        return;
        }
    }
	
}
