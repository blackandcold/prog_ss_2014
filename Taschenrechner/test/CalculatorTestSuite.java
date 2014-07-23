import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;


@RunWith(Suite.class)
@SuiteClasses({ BasicCalculatorTest.class, BuilderTest.class, CalculatorTest.class, ParserTest.class,
    PrimalityTestBuilderTest.class, ReadWriteCalculatorTest.class, UITest.class })
public class CalculatorTestSuite {

}
