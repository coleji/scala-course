def pack[T](xs: List[T]): List[List[T]] = xs match {
	case Nil => Nil
	case x :: _ =>
		val (first, rest) = xs.span(y => y == x)
		first :: pack(rest)
}

def encode[T](xs: List[T]): List[(T, Int)] = xs match {
	case Nil => Nil
	case _ :: _ => pack(xs).map(l => (l.head, l.length))
}

val a = List(1,1,1,2,2,3,3,3,3,3,4,4,1,1)

val b = pack(a)

val c = encode(a)