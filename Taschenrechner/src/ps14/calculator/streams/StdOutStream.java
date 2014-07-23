package ps14.calculator.streams;

/**
 * Wrapper around System.out, writes integers as characters.
 */
public class StdOutStream implements IStream {

	@Override
    public int read() {
	    throw new UnsupportedOperationException();
    }

	@Override
    public void write(int i) {
	    System.out.print(Character.toString((char) i));
    }
	
}
