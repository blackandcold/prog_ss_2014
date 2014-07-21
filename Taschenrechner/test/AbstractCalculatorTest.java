import static org.junit.Assert.assertEquals;

import org.junit.Assert;
import org.junit.Before;

import ps14.calculator.Calculator;
import ps14.calculator.Context;
import ps14.calculator.parser.ParseException;


public abstract class AbstractCalculatorTest {
	protected Calculator calculator;
	
	@Before
	public void setUp() {
		calculator = new Calculator();
	}
	
	protected void expect(String input, String output) {
		Context c;
        try {
	        c = calculator.run(input);
        } catch (ParseException e) {
	        e.printStackTrace();
	        Assert.fail();
	        return;
        }
		
        assertEquals(output, c.toString());
	}
	
	protected void expectStep(String input, String output) {
		Context c;
        try {
	        c = calculator.parse(input);
        } catch (ParseException e) {
	        e.printStackTrace();
	        Assert.fail();
	        return;
        }
		calculator.executeStep();
        assertEquals(output, c.toString());
	}
}
