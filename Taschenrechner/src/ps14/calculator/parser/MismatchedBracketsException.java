package ps14.calculator.parser;

public class MismatchedBracketsException extends ParseException {
    private static final long serialVersionUID = 1L;

	public MismatchedBracketsException() {
		super("Mismatched Brackets!");
    }
}
