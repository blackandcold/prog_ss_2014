package ps14.calculator.streams;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;

/**
 * An input stream wrapper.
 * 
 * The default constructor wraps System.in.
 * Use a String to mock stdin input.
 */
public class InStream implements IStream {
	private InputStream is;
	
	public InStream() {
	    this(System.in);
    }
	
	public InStream(String input) {
		this(new ByteArrayInputStream(input.getBytes()));
	}
	
	public InStream(InputStream is) {
		this.is = is;
	}
	
	@Override
	public int read() {
		try {
	        return is.read();
        } catch (IOException e) {
	        return -1;
        }
	}

	@Override
	public void write(int i) {
		throw new UnsupportedOperationException("Cannot write to stdin");
	}

}
