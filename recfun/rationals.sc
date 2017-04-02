class Rational(x: Int, y: Int) {
	def numer = x
	def denom = y
	def add(r: Rational) =
		new Rational(numer * r.denom + r.numer * denom,
			denom * r.denom)
	def mul(r: Rational) =
		new Rational(x * r.numer, y * r.denom)

	def neg = new Rational (-1 * x, y)

	def sub(r: Rational) = add(r.neg)
	override def toString = numer + "/" + denom
}

val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)

x.add(y).mul(z).sub(new Rational(1, 42))