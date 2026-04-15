import mohs.Mohs
import mohs.prettyPrint
import mohs.UFunc
import mohs.Box

class MathSuite extends munit.FunSuite {
  val a = Mohs.List(1, 2, 3)
  val b = Mohs.List(4, 5, 6)
  val A = Mohs.List(
    -1, 2, 3,
    -2, -1, -3,
    5, -5, 4,
  ).reshape(3, 3)
  val B = Mohs.List(
    -5, -3, 4,
    3, -5, -4,
    1, -1, 6,
  ).reshape(3, 3)

  test("addition") {
    val obtained = a + b
    val expected = Mohs.List(5, 7, 9)
    assert(obtained.equals(expected), "values are not the same")
  }

  test("subtraction") {
    val obtained = a - b
    val expected = Mohs.List(-3, -3, -3)
    assert(obtained.equals(expected), "values are not the same")
  }

  test("multiplication") {
    val obtained = a * b
    val expected = Mohs.List(4, 10, 18)
    assert(obtained.equals(expected), "values are not the same")
  }

  test("division") {
    val obtained = a / b
    val expected = Mohs.List(1/4.0, 2/5.0, 3/6.0)
    assert(obtained.equals(expected), "values are not the same")
  }

  test("maximum") {
    val obtained = a.maximum(b)
    val expected = b
    assert(obtained.equals(expected), "values are not the same")
  }

  test("minimum") {
    val obtained = a.minimum(b)
    val expected = a
    assert(obtained.equals(expected), "values are not the same")
  }

  val testMap = Map(
    "list x list" -> (a, b),
    "list x num" -> (a, 100),
    "list x mat" -> (a, A),
    "mat x list" -> (A, b),
    "mat x mat" -> (A, B),
    "mat x num" -> (A, 100),
  )

  for (name,(x, y)) <- testMap do {
    test(f"addition $name") { a + b }
    test(f"subtraction $name") { a - b }
    test(f"multiplication $name") { a * b }
    test(f"division $name") { a / b }
    test(f"maximum $name") { a.maximum(b) }
    test(f"minimum $name") { a.minimum(b) }
  }
}

class AggSuite extends munit.FunSuite {
  val a = Mohs.List(1, 2, 3, 4, -5, -6, -7, -8)
  val A = Mohs.List(
    1, 2, 3,
    -2, -1, -3,
    5, -5, 4,
    -1, 2, -3,
  ).reshape(4, 3)

  test("sum") {
    val obtained = a.sum
    val expected = 1+2+3+4-5-6-7-8
    assertEquals(obtained, expected)
  }

  test("min") {
    val obtained = a.min
    val expected = -8
    assertEquals(obtained, expected)
  }

  test("max") {
    val obtained = a.max
    val expected = 4
    assertEquals(obtained, expected)
  }

  test("sum axis") {
    val obtained = A.axis(2).sum
    val expected = Mohs.List(1+2+3, -2-1-3, 5-5+4, -1+2-3)
    assert(obtained.equals(expected), "values are not the same")
  }

  test("min axis") {
    val obtained = A.axis(2).min
    val expected = Mohs.List(1, -3, -5, -3)
    assert(obtained.equals(expected), "values are not the same")
  }

  test("max axis") {
    val obtained = A.axis(2).max
    val expected = Mohs.List(3, -1, 5, 2)
    assert(obtained.equals(expected), "values are not the same")
  }

  test("reduce") {
    val obtained = a.reduce(_-_)
    val expected = 1-2-3-4+5+6+7+8
    assertEquals(obtained, expected)
  }

  test("reduce axis") {
    val obtained = A.axis(2).reduce(_-_)
    val expected = Mohs.List(1-2-3, -2+1+3, 5+5-4, -1-2+3)
    assert(obtained.equals(expected), "values are not the same")
  }

  test("scan") {
    val obtained = a.scan(0)(_+_)
    val expected = Mohs.List(0, 1, 3, 6, 10, 5, -1, -8, -16)
    assert(obtained.equals(expected), "values are not the same")
  }

  test("scan axis") {
    val obtained = A.axis(2).scan(0)(_+_)
    val expected = Mohs.List(
      0, 1, 3, 6,
      0, -2, -3, -6,
      0, 5, 0, 4,
      0, -1, 1, -2,
    ).reshape(4, 4)
    assertEquals(obtained.toString(), expected.toString())
  } 
}

class ShapeSuite extends munit.FunSuite {
  val a = Mohs.List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)

  def assertShape(a: Array[Int], b: Array[Int]) =
    assertEquals(a.toSeq, b.toSeq)

  test("reshape") {
    val obtained = a.reshape(4, 3).shape
    val expected = Array(4, 3)
    assertShape(obtained, expected)
  }

  test("ravel") {
    val obtained = a.reshape(4, 3).ravel.shape
    val expected = Array(12)
    assertShape(obtained, expected)
  }

  test("transpose") {
    val obtained = a.reshape(4, 3).transpose.shape
    val expected = Array(3, 4)
    assertShape(obtained, expected)
  }

  test("transpose 3d") {
    val obtained = a.reshape(2, 2, 3).transpose.shape
    val expected = Array(2, 3, 2)
    assertShape(obtained, expected)
  }

  test("reorder axis") {
    val obtained = a.reshape(3, 4, 6).reorderAxis(2, 0, 1).shape
    val expected = Array(4, 6, 3)
    assertShape(obtained, expected)
  }

  test("repeat") {
    val obtained = a.reshape(3, 4).repeat(2).shape
    val expected = Array(6, 4)
    assertShape(obtained, expected)
  }

  test("repeat axis") {
    val obtained = a.reshape(3, 4).axis(2).repeat(2).shape
    val expected = Array(3, 8)
    assertShape(obtained, expected)
  }

  test("ravel axis") {
    val obtained = a.reshape(2, 2, 3).axis(2).ravel.shape
    val expected = Array(2, 6)
    assertShape(obtained, expected)
  }

  test("reshape axis") {
    val obtained = a.reshape(2, 2, 3).axis(2).reshape(6, 2).shape
    val expected = Array(2, 6, 2)
    assertShape(obtained, expected)
  }
}

class SelectionSuite extends munit.FunSuite {
  val a = Mohs.List(1, 2, 3, 4, -5, -6, -7, -8)
  val A = Mohs.List(
    1, 2, 3,
    -2, -1, -3,
    5, -5, 4,
    -1, 2, -3,
  ).reshape(4, 3)

  test("take") {
    val obtained = A.take(2)
    val expected = Mohs.List(
      1, 2, 3,
      -2, -1, -3
    ).reshape(2, 3)
    assertEquals(obtained.toString(), expected.toString())
  }

  test("take axis") {
    val obtained = A.axis(2).take(2)
    val expected = Mohs.List(
      1, 2,
      -2, -1,
      5, -5,
      -1, 2,
    ).reshape(4, 2)
    assertEquals(obtained.toString(), expected.toString())
  }

  test("drop") {
    val obtained = A.drop(2)
    val expected = Mohs.List(
      5, -5, 4,
      -1, 2, -3,
    ).reshape(2, 3)
    assertEquals(obtained.toString(), expected.toString())
  }

  test("drop axis") {
    val obtained = A.axis(2).drop(2)
    val expected = Mohs.List(
      3,
      -3,
      4,
      -3,
    ).reshape(4, 1)
    assertEquals(obtained.toString(), expected.toString())
  }

  test("take many") {
    val obtained = A.take(2, 2)
    val expected = Mohs.List(
      1, 2,
      -2, -1,
    ).reshape(2, 2)
    assertEquals(obtained.toString(), expected.toString())
  }

  test("drop many") {
    val obtained = A.drop(2, 2)
    val expected = Mohs.List(
      4,
      -3,
    ).reshape(2, 1)
    assertEquals(obtained.toString(), expected.toString())
  }

  test("cells") {
    val obtained = A.cells(1).toArray
    assertEquals(obtained.length, 4)
    assertEquals(obtained(0).toString(), Mohs.List(1, 2, 3).toString())
    assertEquals(obtained(1).toString(), Mohs.List(-2, -1, -3).toString())
    assertEquals(obtained(2).toString(), Mohs.List(5, -5, 4).toString())
    assertEquals(obtained(3).toString(), Mohs.List(-1, 2, -3).toString())
  }

  test("boolean select") {
    val obtained = A(Mohs.List(true, false, true, false))
    val expected = Mohs.List(
      1, 2, 3,
      5, -5, 4,
    ).reshape(2, 3)
    assertEquals(obtained.toString(), expected.toString())
  }

  test("int select") {
    val obtained = A(Mohs.List(0, 1, 3, 2, 0, 0).reshape(2, 3))
    val expected = Mohs.List(
      1, 2, 3,
      -2, -1, -3,
      -1, 2, -3,
      5, -5, 4,
      1, 2, 3,
      1, 2, 3,
    ).reshape(2, 3, 3)
    assertEquals(obtained.toString(), expected.toString())
  }
}

class SearchSuite extends munit.FunSuite {
  val a = Mohs.String("helloworld")
  val A = Mohs.String("loremipsumdolor").reshape(3, 5)

  test("classify") {
    val obtained = a.classify()
    val expected = Mohs.List(0, 1, 2, 2, 3, 4, 3, 5, 2, 6)
    assertEquals(obtained.toString(), expected.toString())
  }

  test("occurence count") {
    val obtained = a.occurrenceCount()
    val expected = Mohs.List(0, 0, 0, 1, 0, 0, 1, 0, 2, 0)
    assertEquals(obtained.toString(), expected.toString())
  }

  test("mark firsts") {
    val obtained = a.markFirsts()
    val expected = Mohs.List(true, true, true, false, true, true, false, true, false, true)
    assertEquals(obtained.toString(), expected.toString())
  }

  test("deduplicate") {
    val obtained = a.deduplicate()
    val expected = Mohs.String("helowrd")
    assertEquals(obtained.toString(), expected.toString())
  }

  test("isin") {
    val obtained = a.isIn("low")
    val expected = Mohs.List(false, false, true, true, true, true, true, false, true, false)
    assertEquals(obtained.toString(), expected.toString())
  }

  test("index of") {
    val obtained = a.indexOf(Mohs.String("low"))
    val expected = Mohs.List(2,4,5)
    assertEquals(obtained.toString(), expected.toString())
  }

  test("find") {
    val obtained = a.find(Mohs.String("low"))
    val expected = Mohs.List(false, false, false, true, false, false, false, false)
    assertEquals(obtained.toString(), expected.toString())
  }


  test("classify 2d") {
    val obtained = A.classify()
    val expected = Mohs.List(0, 1, 2)
    assertEquals(obtained.toString(), expected.toString())
  }

  test("occurence count 2d") {
    val obtained = A.occurrenceCount()
    val expected = Mohs.List(0, 0, 0)
    assertEquals(obtained.toString(), expected.toString())
  }

  test("mark firsts 2d") {
    val obtained = A.markFirsts()
    val expected = Mohs.List(true, true, true)
    assertEquals(obtained.toString(), expected.toString())
  }

  test("deduplicate 2d") {
    val obtained = A.deduplicate()
    val expected = A
    assertEquals(obtained.toString(), expected.toString())
  }

  test("isin 2d") {
    val obtained = A.isIn("low")
    val expected = Mohs.List(
      true, true, false, false, false,
      false, false, false, false, false,
      false, true, true, true, false,
    ).reshape(3,5)
    assertEquals(obtained.toString(), expected.toString())
  }

  test("index of 2d") {
    val obtained = A.indexOf(Mohs.String("loremdolor").reshape(2,5))
    val expected = Mohs.List(0, 2)
    assertEquals(obtained.toString(), expected.toString())
  }

  test("find 2d") {
    val obtained = A.find(Mohs.String("lor"))
    val expected = Mohs.List(
      true, false, false, 
      false, false, false, 
      false, false, true
    ).reshape(3,3)
    assertEquals(obtained.toString(), expected.toString())
  }
}

class LinRegSuite extends munit.FunSuite {
  val niter = 100
  val alpha = 0.09
  val N = 100

  for (i <- 1 to 5) {
    test(s"Linear regression") {
      
      val X = Mohs.Random(N, 3)

      val true_w = Mohs.Random(3)*50
      val true_bias = Mohs.Random()
      val y = (X dot true_w) + true_bias + Mohs.Random(N)*0.01

      var w = Mohs.Fill(1.0)(3)
      var bias = Mohs.Random()*0.0

      for (_ <- 0 until niter) {
        val y_hat = (X dot w) + bias
        val error = y_hat - y
        val grad = (X.transpose dot error) / N
        val grad_bias = error.sum / N
        w = w - grad * alpha
        bias = bias - grad_bias * alpha
      }

      val sum = ((w-true_w)**2).sum + ((bias-true_bias)**2).sum

      assert(sum / N < 1.0)    
    }
  }
}

class KMeansSuit extends munit.FunSuite {

  def updateCentroids(points: Mohs[Double], labels: Mohs[Int], k: Int): Mohs[Double] =
    val N = labels.length

    val mask = UFunc.toDouble(Mohs.Range(0, k).tile(N) == labels)
    val sums = (mask~3 * points~2).sum(1)

    val counts = mask.sum(1) 

    sums / counts

  test("K-Means") {
    val points = Mohs.Random(100, 2)
    var centroids = Mohs.Random(3, 2)

    for _ <- 0 until 100 do
      val distances = ((points~2 - centroids~1) ** 2).sum(3)
      val labels = distances.argmin(2)
      centroids = updateCentroids(points, labels, 3)  
  }
}

class PCASuit extends munit.FunSuite {
  import UFunc.*
  import breeze.linalg

  def breeze2mohs[V:scala.reflect.ClassTag](m: linalg.DenseMatrix[V]) =
    Mohs(Array(m.rows, m.cols),m.data)
  

  test("PCA") {
    

    val X = Mohs.Random(2, 100)
    val mean = X - X.mean(2)
    val std = sqrt(sum(mean.abs ** 2) / X.length)
    val Xstd = (X - mean) / std

    val cov = (Xstd.transpose <*> Xstd) / (X.shape(1) - 1)

    val mat = new linalg.DenseMatrix(rows=100, cols=100, data=cov.values)

    val linalg.svd.SVD(u, s, breeze_vt) = linalg.svd(mat)

    val vt = breeze2mohs(breeze_vt)

    val scores = Xstd <*> vt.transpose
  }
}

class DataProcessSuit extends munit.FunSuite {
  case class User(name: String, age: Int, email: String)

  val users = Mohs.List(
    User(" alice ", 17, "ALICE@example.com"),
    User("BOB", 25, "bob@example.com"),
    User("   charlie", 30, "CHARLIE@EXAMPLE.COM"),
    User("david  ", 15, "david@example.com"),
  )
  
  test("processing") {

    val clean = users.collect { case User(name,age,email) if age >= 18 =>
      User(name.strip().toLowerCase().capitalize, age, email.toLowerCase())
    }

  }
}