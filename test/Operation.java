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

/*    public static void main(String args[]) {
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
