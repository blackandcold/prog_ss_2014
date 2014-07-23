import org.junit.Before;
import org.junit.Test;

import ps14.calculator.builder.PrimalityTestBuilder;


public class PrimalityTestBuilderTest extends AbstractCalculatorTest {
	private PrimalityTestBuilder primes;
	
	@Before
	@Override
	public void setUp() {
		super.setUp();
		primes = new PrimalityTestBuilder();
	};
	
	@Test
	public void test_primes() {
		String p = primes.getPrimalityTestOperator();
		expect("1" + p, "0 ^");
		expect("2" + p, "1 ^");
		expect("3" + p, "1 ^");
		expect("4" + p, "0 ^");
		expect("5" + p, "1 ^");
		expect("7" + p, "1 ^");
		expect("8" + p, "0 ^");
		expect("9" + p, "0 ^");
		expect("11" + p, "1 ^");
	}
	
	@Test
	public void test_nonpositiveNumbers() {
		String p = primes.getPrimalityTestOperator();
		expect("0" + p, "0 ^");
		expect("2~" + p, "0 ^");
	}
	
	@Test
	public void test_primes_to_1000() {
		String p = primes.getPrimalityTestOperator();
		for (int i = 1; i <= 1000; i++) {
			expect(i + p, (isPrime(i) ? 1 : 0) + " ^");
		}
	}
	
	
	
	
	private boolean isPrime(long n) {
		if (n == 1) return false;
	    // fast even test.
	    if(n > 2 && (n & 1) == 0)
	       return false;
	    // only odd factors need to be tested up to n^0.5
	    for(int i = 3; i * i <= n; i += 2)
	        if (n % i == 0) 
	            return false;
	    return true;
	}
}
