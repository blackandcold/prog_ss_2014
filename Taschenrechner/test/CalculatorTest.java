import org.junit.Test;


public class CalculatorTest extends AbstractCalculatorTest {
	@Test
	public void test_example_if_false() {
		expect("0 ^ [9~][9][3c4d1+da]a", "-9 ^");
	}
	
	@Test
	public void test_example_if_true() {
		expect("1 ^ [9~][9][3c4d1+da]a", "9 ^");
	}
	
	@Test
	public void test_fac_3() {
		expect("3 ^ [2c1 3c-1c1=3c[][3c4d1+da]a2d*]2c3d2ca2d", "6 ^");
	}
	
	@Test
	public void test_fac_5() {
		expect("5 ^ [2c1 3c-1c1=3c[][3c4d1+da]a2d*]2c3d2ca2d", "120 ^");
	}
	
}
