package ps14.calculator;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import ps14.calculator.builder.UIBuilder;
import ps14.calculator.elements.operators.ExitOperator;
import ps14.calculator.parser.ParseException;
import ps14.calculator.parser.Parser;
import ps14.calculator.streams.GUIDisplay;
import ps14.calculator.streams.IStream;
import ps14.calculator.streams.StdOutStream;

public class Calculator {
	private Context context;
	public boolean printStateEveryStep = false;
	
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
	
	
	public static void main(String[] args) throws IOException {
		List<String> argList = Arrays.asList(args);
	    
	    if (argList.contains("--help") || argList.contains("-h")) {
	    	System.out.println("Usage: Calculator [options] [file]\n");
	    	System.out.printf("%1$-20s %2$s\n", "-v --verbose", "Verbose (Print state every step)");
	    	System.out.printf("%1$-20s %2$s\n", "-i --interactive", "Interactive mode (Present the user interface)");
	    	System.out.printf("%1$-20s %2$s\n", "-o --stdout", "Print output to stdout instead of display");
	    	System.out.printf("%1$-20s %2$s\n", "-x", "Wait after program is finished");
	    	return;
	    }
	    
	    IStream outStream;
	    Calculator calc = new Calculator();
	    
	    if (argList.contains("-o") || argList.contains("--stdout")) {
	    	outStream = new StdOutStream();
	    } else {
	    	outStream = new GUIDisplay();
	    }
	    
	    boolean wait = false;
	    if (argList.contains("-x")) {
	    	wait = true;
	    }
	    
	    String code;
	    Optional<String> filePath = argList.stream().filter(x -> !x.startsWith("-")).findFirst();
	    
	    if (filePath.isPresent()) {
		    try {
	             code = new String(Files.readAllBytes(Paths.get(filePath.get())));
	        } catch (IOException e) {
	            System.out.println("Could not read file \"" + filePath.get() + "\".");
	            return;
	        }
	    } else if (argList.contains("-i") || argList.contains("--interactive")) {
	    	code = new UIBuilder().getUI();
	    } else {
	    	code = new BufferedReader(new InputStreamReader(System.in)).readLine();
	    	calc.printStateEveryStep = true;
	    }

	    if (argList.contains("--verbose") || argList.contains("-v")) {
	    	calc.printStateEveryStep = true;
	    }
	    
	    try {
	        calc.parse(code);
	        calc.getContext().setOutputStream(outStream);
	        calc.run();
        } catch (ParseException e) {
	        System.err.println("Parsing error: " + e.getMessage());
        } catch (CalculatorException e) {
        	System.err.println("Error: " + e.getMessage());
		}
	    
	    if (wait) {
	    	System.in.skip(System.in.available());
	    	System.in.read();
	    }
	    
	    System.exit(0);
    }
	
}
