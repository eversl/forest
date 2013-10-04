package forest;

import java.lang.reflect.Field;
import java.lang.reflect.Method;

public class FindClasspath {

	/**
	 * @param args
	 * @throws NoSuchFieldException 
	 * @throws SecurityException 
	 * @throws NoSuchMethodException 
	 */
	public static void main(String[] args) throws SecurityException, NoSuchFieldException, NoSuchMethodException {
		try {
			Class<?> cls1 = Class.forName("java.lang.System");
			System.out.print(cls1.getName());
			System.out.print("\n");
			System.out.print(cls1.getPackage());
			System.out.print("\n");
			Field fld1 = cls1.getDeclaredField("out");
			System.out.print(fld1.getName());
			System.out.print("\n");
			Class<?> cls2 = fld1.getType();
			System.out.print(cls2);
			Method meth1 = cls1.getDeclaredMethod("println", "Hello, World".getClass());
			
		} catch (ClassNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	
		
	}

}
