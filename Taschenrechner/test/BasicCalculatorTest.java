import org.junit.Test;

import ps14.calculator.CalculatorException;
import ps14.calculator.parser.ParseException;


public class BasicCalculatorTest extends AbstractCalculatorTest {
	
	@Test
	public void test_operators() {
		expect("1 2 3 4+*-", "13 ^");
	}
	
	@Test
	public void test_block() {
		expect("4 3[2*]a+", "10 ^");
	}
	
	@Test(expected=CalculatorException.class)
	public void test_binaryOperator_exception() throws ParseException {
		calculator.run("4 1 &"); 
	}
	
	@Test(expected=CalculatorException.class)
	public void test_notEnoughArguments() throws ParseException {
		calculator.run("1 &"); 
	}
	
	@Test(expected=CalculatorException.class)
	public void test_unexpectedArgument() throws ParseException {
		calculator.run("[] 1 &"); 
	}
	
	@Test
	public void test_negate() {
		expect("42 ~", "-42 ^");
	}
	
	@Test(expected=CalculatorException.class)
	public void test_negate_block() {
		expect("[3] ~", "error");
	}
	
	@Test
	public void test_and() {
		expect("1 1 &", "1 ^");
		expect("1 0 &", "0 ^");
		expect("0 1 &", "0 ^");
		expect("0 0 &", "0 ^");
	}
	
	@Test
	public void test_or() {
		expect("1 1 |", "1 ^");
		expect("1 0 |", "1 ^");
		expect("0 1 |", "1 ^");
		expect("0 0 |", "0 ^");
	}
	
	@Test
	public void test_equals() {
		expect("1 1 =", "1 ^");
		expect("1 3 =", "0 ^");
	}
	
	@Test(expected=CalculatorException.class)
	public void test_equals_block() {
		expect("[1] 1 =", "error");
	}
	
	@Test
	public void test_remainder() {
		expect("4 2%", "2 ^");
	}
	
	@Test
	public void test_comparison_greater() {
		expect("2 4>", "1 ^");
		expect("4 4>", "0 ^");
		expect("6 4>", "0 ^");
	}
	
	@Test
	public void test_comparison_less() {
		expect("4 2<", "1 ^");
		expect("2 2<", "0 ^");
		expect("1 2<", "0 ^");
	}
	
	@Test
	public void test_addition() {
		expect("4 2 +", "6 ^");
	}
	
	@Test
	public void test_subtraction() {
		expect("4 2 -", "-2 ^");
	}
	
	@Test
	public void test_division() {
		expect("2 4 /", "2 ^");
		expect("3 10/", "3 ^");
	}
	
	@Test
	public void test_modulo() {
		expect("2 4 %", "0 ^");
		expect("3 10%", "1 ^");
	}
	
	@Test(expected=CalculatorException.class)
	public void test_divByZero() throws ParseException {
		calculator.run("0 4/");
	}
	
	@Test(expected=CalculatorException.class)
	public void test_divByZero_modulo() throws ParseException {
		calculator.run("0 4%");
	}
	
	
	@Test
	public void test_applyInteger() {
		expect("2 a", "2 ^");  // (apply has no effect on non-blocks)
	}
	
	@Test
	public void test_applyBlock() {
		expect("[3 2] a", "3 2 ^");
		expect("[3 3 +] a", "6 ^");
		expect("3[2*]a", "6 ^");
	}
	
	@Test
	public void test_copy() {
		expect("1 2 3 4 ^1c", "1 2 3 4 4 ^");
		expect("1 2 3 4 ^2c", "1 2 3 4 3 ^");
		expect("1 2 3 4 ^3c", "1 2 3 4 2 ^");
		expect("1 2 3 4 ^4c", "1 2 3 4 1 ^");
	}
	
	@Test
	public void test_copy_block() {
		expect("[1] 1c 2g", "[1] [1 2] ^");
	}
	
	@Test(expected=CalculatorException.class)
	public void test_copy_nonpositiveIndex() {
		expect("1 2 3~c", "error");
	}
	
	@Test(expected=CalculatorException.class)
	public void test_copy_outOfRangeIndex() {
		expect("1 2 3c", "error");
	}
	
	@Test
	public void test_delete() {
		expect("1 2 3 4 ^1d", "1 2 3 ^");
		expect("1 2 3 4 ^2d", "1 2 4 ^");
		expect("1 2 3 4 ^3d", "1 3 4 ^");
		expect("1 2 3 4 ^4d", "2 3 4 ^");
	}
	
	@Test(expected=CalculatorException.class)
	public void test_delete_nonpositiveIndex() {
		expect("1 2 3~d", "error");
	}
	
	@Test(expected=CalculatorException.class)
	public void test_delete_outOfRangeIndex() {
		expect("1 2 3d", "error");
	}
	
	
	@Test
	public void test_group_twoBlocks(){
		expectStep("[1+][2*]^g", "[1 + 2 *] ^");
	}
	
	@Test
	public void test_group_block_int(){
		expectStep("[1 2]3^g", "[1 2 3] ^");
	}
	
	@Test
	public void test_group_int_block(){
		expectStep("4[3 -]^g", "[4 3 -] ^");
	}
	
	@Test
	public void test_group_int_int(){
		expectStep("42 27^g", "[42 27] ^");
	}
	
	@Test
	public void test_build_block() {
		expect("[1 2]b", "[[1 2]] ^");
	}
	
	@Test
	public void test_build_op() {
		expect((int)'a' + "b", "[a] ^");
	}
	
	@Test(expected=CalculatorException.class)
	public void test_build_op_unknown() {
		expect((int)'y' + "b", "error");
	}
	
	@Test(expected=CalculatorException.class)
	public void test_build_op_unknown2() {
		expect("1b", "error");
	}
	
	@Test
	public void test_exit() {
		expect("x 3 2 1", "^ 3 2 1");
		expect("4 3 + x 2*", "7 ^ 2 *");
	}
}
