package ps14.calculator.streams;

import java.awt.Color;
import java.awt.Font;

import javax.swing.BorderFactory;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JTextArea;
import javax.swing.border.Border;

public class GUIDisplay implements IStream {
	private Display display;
	private JFrame frame;
	private JTextArea textArea;
	
	private Color background = new Color(0xD7D4B7);
	private Color foreground = new Color(0x3B2322);
	
	public GUIDisplay() {
	    display = new Display();
	    
	    frame = new JFrame();
	    frame.setTitle("Display");
	    frame.setResizable(false);
	    
	    JPanel contentPanel = new JPanel();
	    Border padding = BorderFactory.createEmptyBorder(10, 10, 10, 10);
	    contentPanel.setBorder(padding);
	    frame.setContentPane(contentPanel);
	    
	    textArea = new JTextArea(display.getLines(), display.getColumns());
	    textArea.setEditable(false);
	    textArea.setFont(new Font("Courier New", Font.BOLD, 20));
	    textArea.setBackground(background);
	    contentPanel.setBackground(background);
	    textArea.setForeground(foreground);
	    
	    contentPanel.add(textArea);
	    frame.setVisible(true);
	    frame.pack();
    }
	
	@Override
	public int read() {
		return display.read();
	}

	@Override
	public void write(int i) {
		display.write(i);
		textArea.setText(display.toString());
	}

}
