import org.junit.Before;
import org.junit.Test;

import ps14.calculator.builder.UIBuilder;


public class UITest extends AbstractCalculatorTest {
	private String ui;
	
	@Before
	@Override
	public void setUp() {
		super.setUp();
		UIBuilder uiBuilder = new UIBuilder();
		uiBuilder.silence();
		ui = uiBuilder.getUI();
	};
	
	@Test
	public void test_ui_write() {
		expect(ui, null, "90 7 + w", "a\n");
	}
	
	@Test
	public void test_ui_newline() {
		expect(ui, null, "49w\n50w51w", "1\n23\n");
	}
	
	@Test
	public void test_ui_exit() {
		expect(ui, null, "97w98w x 99w", "ab");
	}
	
	@Test
	public void test_ui_blocks() {
		expect(ui, null, "[49 [2*]a]aw", "b\n");
	}

}
