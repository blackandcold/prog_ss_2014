import static org.junit.Assert.assertEquals;

import org.junit.Assert;
import org.junit.Before;

import ps14.calculator.Calculator;
import ps14.calculator.Context;
import ps14.calculator.parser.ParseException;
import ps14.calculator.streams.InStream;


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
	
	protected void expect(String input, String output, String streamInput, String streamOutput) {
		Context c;
        try {
	        c = calculator.parse(input);
        } catch (ParseException e) {
	        e.printStackTrace();
	        Assert.fail();
	        return;
        }
        
        c.setInputStream(new InStream(streamInput));
        calculator.run();
        if(output != null) assertEquals(output, c.toString());
        if(streamOutput != null) assertEquals(streamOutput, c.getOutputStream().toString());
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
