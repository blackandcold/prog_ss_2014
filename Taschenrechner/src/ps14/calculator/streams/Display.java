package ps14.calculator.streams;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * A display with limited size.
 */
public class Display implements IStream {
	private static final int LINES = 6;
	private static final int COLUMNS = 64;
	
	private List<StringBuffer> lines = new ArrayList<>();
	
	public Display() {
	    addLine();
    }
	
	private void addLine() {
		lines.add(new StringBuffer());
		if (lines.size() > LINES)
			lines.remove(0);
	}
	
	@Override
	public int read() {
		throw new UnsupportedOperationException();
	}

	@Override
	public void write(int i) {
		char ch = (char) i;
		
		if (ch == '\n') {
			addLine();
			return;
		}
		
		StringBuffer line = lines.get(lines.size()-1); 
		line.append(ch);
		if (line.length() > COLUMNS)
			line.deleteCharAt(0);
	}
	
	@Override
	public String toString() {
		return lines.stream().map(l->l.toString()).collect(Collectors.joining("\n"));
	}

}
