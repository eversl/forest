package test.Operation;

language "generics.java.rules.forest";

language "enum.java.rules.forest";
language "enum.java.patterns.forest";
 
import java.util.*;

public enum Operation {
    plus("+") {
        double eval(double x, double y) { return x + y; }
    },
    minus("-") {
        double eval(double x, double y) { return x - y; }
    },
    times("*") {
        double eval(double x, double y) { return x * y; }
    },
    divided_by("/") {
        double eval(double x, double y) { return x / y; }
    };

	private String opStr;
	
	Operation(String opStr) {this.opStr = opStr;}
	
    abstract double eval(double x, double y);

    /* public static void main(String args[]) {
        double x = Double.parseDouble(args[0]);
        double y = Double.parseDouble(args[1]);

        for (Operation op : Operation.values()) {
            System.out.println(x + " " + op + " " + y + " = " + op.eval(x, y));
        }
    } */
}
// Running this program produces the following output:
// java Operation 2.0 4.0
// 2.0 plus 4.0 = 6.0
// 2.0 minus 4.0 = -2.0
// 2.0 times 4.0 = 8.0
// 2.0 divided_by 4.0 = 0.5


////////// the code this should translate into /////////////

// Typesafe enum with behaviors attached to constants
public abstract class Operation extends Enum<Operation> {

	// Perform arithmetic operation represented by this constant
    abstract double eval(double x, double y);
    
    public static final Operation plus = new Operation("plus", 0, "+") {
        double eval(double x, double y) { return x + y; }
    };

    public static final Operation minus = new Operation("minus", 1+0, "-") {
        double eval(double x, double y) { return x - y; }
    };
    public static final Operation times = new Operation("times", 1+1+0, "*") {
        double eval(double x, double y) { return x * y; }
    };
    public static final Operation divided_by = new Operation("divided_by", 1+1+1+0, "/") {
        double eval(double x, double y) { return x / y; }
    }; 

    private static final Operation[] $VALUES = { plus, minus, times, divided_by };
    
	public static Operation[] values() {
		return (Operation[]) $VALUES.clone();
	}

    public static Operation valueOf(String name) {
    	return (Operation) java.lang.Enum.valueOf(Operation.class, name);
    }
        
	private String opStr;
	
	protected Operation(String name, int ordinal, String opStr) { 
    	super(name, ordinal);
    	this.opStr = opStr;
    }

}