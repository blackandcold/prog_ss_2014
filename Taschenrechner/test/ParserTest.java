import static org.junit.Assert.assertEquals;

import org.junit.Before;
import org.junit.Test;

import ps14.calculator.parser.MismatchedBracketsException;
import ps14.calculator.parser.ParseException;
import ps14.calculator.parser.Parser;


public class ParserTest {
	private Parser parser;
	
	@Before
	public void setUp() {
		this.parser = new Parser();
	}
	
	@Test(expected=MismatchedBracketsException.class)
	public void test_brackets_mismatched() throws ParseException {
		parser.parse("[4 8");
	}
	
	@Test(expected=MismatchedBracketsException.class)
	public void test_brackets_mismatched2() throws ParseException {
		parser.parse("[4 [8]");
	}
	
	@Test(expected=MismatchedBracketsException.class)
	public void test_brackets_mismatched3() throws ParseException {
		parser.parse("4 8]");
	}
	
	@Test
	public void test_numbers() throws ParseException {
		assertEquals("^ 1 5 - 8 +", parser.parse("1 5 -8+").toString());
	}
	
	@Test
	public void test_operators() throws ParseException {
		assertEquals("^ 1 2 + 3 -", parser.parse("1 2 + 3 -").toString());
	}
	
	@Test
	public void test_operators_noSpaces() throws ParseException {
		assertEquals("^ 1 2 3 4 + - +", parser.parse("1 2 3 4+ -+").toString());
	}
	
	@Test
	public void test_blocks() throws ParseException {
		assertEquals("^ [1 [2 3]]", parser.parse("[1 [2 3]]").toString());
	}
	
	@Test
	public void test_withDataStack() throws ParseException {
		assertEquals("1 2 3 ^ + *", parser.parse("1 2 3 ^ + *").toString());
	}
	
	@Test
	public void test_complex() throws ParseException {
		assertEquals("3 ^ [2 c 1 3 c - 1 c 1 = 3 c [] [3 c 4 d 1 + d a] a 2 d *] 2 c 3 d 2 c a 2 d",
				parser.parse("3^[2c1 3c-1c1=3c[][3c4d1+da]a2d*]2c3d2ca2d").toString());
	}
}
