import org.junit.Test;

import ps14.calculator.builder.Builder;


public class BuilderTest extends AbstractCalculatorTest {
	
	@Test
	public void test_if() {
		expect(Builder.If("0", "1", "2"), "2 ^");
		expect(Builder.If("1", "1", "2"), "1 ^");
	}
	
	@Test
	public void test_if_expression() {
		String readEqualsAsciiFive = "r" + (int)'5' + "="; 
		expect(Builder.If(readEqualsAsciiFive, "1", "2"), "2 ^", "8", "");
		expect(Builder.If(readEqualsAsciiFive, "1", "2"), "1 ^", "5", "");
	}
	
	@Test
	public void test_while() {
		expect(Builder.While("r10=1-", "97w"), "^", "hello\n", "aaaaa");
	}
	
	@Test
	public void test_writeString() {
		expect(Builder.WriteString("Hello!"), "^", "", "Hello!");
	}
	
	@Test
	public void test_appendDigit() {
		expect("7 2 [2c3d10*+]a", "72 ^");
	}
	
	@Test
	public void test_convertToDigit() {
		expect("r"+Builder.parseDigit(), "7 ^", "7", null);
	}
	
	@Test
	public void test_checkIfDigit() {
		String check = Builder.checkIfDigit();
		String readCheck = "r"+check;
		expect(readCheck, "1 ^", "0", null);
		expect(readCheck, "1 ^", "7", null);
		expect(readCheck, "1 ^", "9", null);
		expect(readCheck, "0 ^", "/", null);
		expect(readCheck, "0 ^", ":", null);
	}
	
	@Test
	public void test_checkIfLetter() {
		String check = Builder.checkIfLetter();
		String readCheck = "r"+check;
		expect(readCheck, "0 ^", "0", null);
		expect(readCheck, "0 ^", "@", null);
		expect(readCheck, "1 ^", "A", null);
		expect(readCheck, "1 ^", "Z", null);
		expect(readCheck, "0 ^", "[", null);
		expect(readCheck, "0 ^", "`", null);
		expect(readCheck, "1 ^", "a", null);
		expect(readCheck, "1 ^", "z", null);
		expect(readCheck, "0 ^", "{", null);
	}
	
	@Test
	public void test_convertToOperator() {
		expect("rb", "[a] ^", "a", null);
		expect("rb", "[w] ^", "w", null);
		expect("rb", "[c] ^", "c", null);
		expect("rb", "[d] ^", "d", null);
		expect("rb", "[r] ^", "r", null);
		expect("rb", "[x] ^", "x", null);
	}
	
	@Test
	public void test_checkIfExit() {
		String program = "r [120=]a";
		expect(program, "1 ^", "x", null);
		expect(program, "0 ^", "y", null);
	}
	
	@Test
	public void test_readNumber() {
		expect(Builder.readNumber(), "123 ^", "123", null);
	}
	
	@Test
	public void test_moveDown() {
		expect("5 4 3 2 1" + Builder.MoveDown(1) , "5 4 3 2 1 ^");
		expect("5 4 3 2 1" + Builder.MoveDown(2) , "5 4 3 1 2 ^");
		expect("5 4 3 2 1" + Builder.MoveDown(3) , "5 4 1 3 2 ^");
		expect("5 4 3 2 1" + Builder.MoveDown(4) , "5 1 4 3 2 ^");
		expect("5 4 3 2 1" + Builder.MoveDown(5) , "1 5 4 3 2 ^");
	}
	
	@Test
	public void test_powerOperator() {
		expect("0 3 " + Builder.powerOperator() , "1 ^");
		expect("4 10 " + Builder.powerOperator() , "10000 ^");
	}
	
	@Test
	public void test_digitCountOperator() {
		expect("0 " + Builder.digitCountOperator() , "1 ^");
		expect("9 " + Builder.digitCountOperator() , "1 ^");
		expect("10 " + Builder.digitCountOperator() , "2 ^");
		expect("42 " + Builder.digitCountOperator() , "2 ^");
		expect("99 " + Builder.digitCountOperator() , "2 ^");
		expect("100 " + Builder.digitCountOperator() , "3 ^");
		
		expect("18~ " + Builder.digitCountOperator() , "2 ^");
	}
	
	@Test
	public void test_mostSignificantDigitOperator() {
		expect("0" + Builder.mostSignificantDigitOperator(), "0 ^");
		expect("5~" + Builder.mostSignificantDigitOperator(), "5 ^");
		expect("42" + Builder.mostSignificantDigitOperator(), "4 ^");
		expect("231" + Builder.mostSignificantDigitOperator(), "2 ^");
	}
	
	@Test
	public void test_writeInt() {
		expect("0" + Builder.WriteInt(), "^", "", "0");
		expect("4" + Builder.WriteInt(), "^", "", "4");
		expect("42" + Builder.WriteInt(), "^", "", "42");
		expect("3~" + Builder.WriteInt(), "^", "", "-3");
		expect("18~" + Builder.WriteInt(), "^", "", "-18");
	}
	
	@Test
	public void test_modifyVariableDownTheStack() {
		expect("0 [3 2] 1" + Builder.MoveToTop(3) + "1+" + Builder.MoveDown(3) , "1 [3 2] 1 ^");
	}
	
}
