package ps14.calculator.elements.operators;

import ps14.calculator.elements.IElement;

public class OperatorFactory {
	/**
	 * Returns a new operator represented by the given char code.
	 * If no such operator exists, null is returned.
	 * @param op The character representing the operator
	 */
	public static IElement getOperator(char op) {
		switch(op) {
			case ApplyOperator.OPERATOR:
				return new ApplyOperator();
			case AdditionOperator.OPERATOR:
				return new AdditionOperator();
			case BuildBlockOperator.OPERATOR:
				return new BuildBlockOperator();
			case CopyOperator.OPERATOR:
				return new CopyOperator();
			case DeleteOperator.OPERATOR:
				return new DeleteOperator();
			case DivisionOperator.OPERATOR:
				return new DivisionOperator();
			case EqualsOperator.OPERATOR:
				return new EqualsOperator();
			case ExitOperator.OPERATOR:
				return new ExitOperator();
			case GreaterThanOperator.OPERATOR:
				return new GreaterThanOperator();
			case GroupOperator.OPERATOR:
				return new GroupOperator();
			case LessThanOperator.OPERATOR:
				return new LessThanOperator();
			case LogicalAndOperator.OPERATOR:
				return new LogicalAndOperator();
			case LogicalOrOperator.OPERATOR:
				return new LogicalOrOperator();
			case ModuloOperator.OPERATOR:
				return new ModuloOperator();
			case MultiplicationOperator.OPERATOR:
				return new MultiplicationOperator();
			case NegationOperator.OPERATOR:
				return new NegationOperator();
			case ReadOperator.OPERATOR:
				return new ReadOperator();
			case SubtractionOperator.OPERATOR:
				return new SubtractionOperator();
			case WriteOperator.OPERATOR:
				return new WriteOperator();
			default:
				return null;
		}
	}
	
}
