package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }
/*
  test("times") {
    assert(times(List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd')) === List(("h",1), ("e",1), (",",1), (" ",1), ("w",1), ("o",2), ("r",1), ("l",3), ("d",1)))
  }
*/

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("decode french secret") {
    new TestTrees {
      assert(decodedSecret === List('h', 'u', 'f', 'f', 'm', 'a', 'n', 'e', 's', 't', 'c', 'o', 'o', 'l'))
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("decode and encode a long text should be identity") {
    new TestTrees {
      assert(decode(t2, encode(t2)("badabaabb".toList)) === "badabaabb".toList)
    }
  }

  test("decode and encode a long text after creating tree") {
    val t3 = createCodeTree(List('a','b','a','g','r','5','n','d','j','a','b','a','a','v'))
    new TestTrees {
      assert(decode(t3, encode(t3)("badabaabb".toList)) === "badabaabb".toList)
    }
  }

  test("encode matches quick encode") {
    val t3 = createCodeTree(List('a','b','a','g','r','5','n','d','j','a','b','a','a','v'))
    new TestTrees {
      println(encode(t3)("badabaabb".toList))
      assert(encode(t3)("badabaabb".toList) === quickEncode(t3)("badabaabb".toList))
    }
  }


}
