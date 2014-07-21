package ps14.calculator.parser;

public class ParseException extends Exception {
	private static final long serialVersionUID = 1L;
	
	
	public ParseException() {
	    super();
    }
	
	public ParseException(String msg) {
		super(msg);
	}
}
