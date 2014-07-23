import org.junit.Test;


public class ReadWriteCalculatorTest extends AbstractCalculatorTest {
	
	@Test
	public void test_read() {
		expect("rrr", "97 98 99 ^", "abc", "");
	}
	
	@Test
	public void test_write() {
		expect("116w101w115w116w", "^", "", "test");
	}
	
	@Test
	public void test_readWrite() {
		expect("rwrwrwrwrw", "^", "hello", "hello");
	}
	
}
