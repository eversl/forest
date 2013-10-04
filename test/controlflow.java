class controlflow extends Object {

  void add(int i) {
    int j = 0, k = j;

    j = 1;
    j = j + i;
  }
}

/*

class controlflow extends java.lang.Object{
controlflow();
  Code:
   0:	aload_0
   1:	invokespecial	#1; //Method java/lang/Object."<init>":()V
   4:	return

void add(int);
  Code:
   0:	iconst_1
   1:	istore_2
   2:	iload_2
   3:	iload_1
   4:	iadd
   5:	istore_2
   6:	return

}


  void add(int i) {
    int j;

    j = 1;
    j = j + i;
  }

  -->

  void add(int i) {
    scope (int j) {
      j = 1;
      j = j + i;
    } (DEFAULT);
  }

  -->

void add(int i) {
    defsub _anon1(int j) {
      j = 1;
      j = j + i;
    } 
    sub _anon1(DEFAULT);
  }

*/