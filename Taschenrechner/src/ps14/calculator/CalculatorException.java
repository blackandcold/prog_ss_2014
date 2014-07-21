package ps14.calculator;

public class CalculatorException extends RuntimeException {
    private static final long serialVersionUID = 1L;

	public CalculatorException() {
	    super();
    }
	
	public CalculatorException(String msg) {
		super(msg);
	}
}
