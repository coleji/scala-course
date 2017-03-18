package pack

object session {
	def factorial(x: Int) = {
		def factorialRecurs(x: Int, partialProduct: Int): Int =
			if (x == 0) partialProduct
			else factorialRecurs(x-1, partialProduct * x);
		factorialRecurs(x, 1)
	}                                         //> factorial: (x: Int)Int

	factorial(4)                              //> res0: Int = 24
	factorial(5)                              //> res1: Int = 120
}