package ps14.calculator.streams;

/**
 * Integer stream that a program can read from and write to.
 */
public interface IStream {
	/**
	 * Reads one byte of input.
	 * Characters are returned with their ASCII encoding.
	 * @return int representing the read byte
	 */
	public int read();
	
	/**
	 * Writes an integer to the stream.
	 * Often interpreted as ASCII code.
	 * @param i Integer to write to the stream.
	 */
	public void write(int i);
}
