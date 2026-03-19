import scala.annotation.static
import mohs.{Mohs, MohsInt, Windows, prettyPrint}
import mohs.given
// import mohs.MohsNumericOps.*

given Conversion[Int, Mohs[Int]] = MohsInt(_)
given Conversion[String, Mohs[Char]] = x => mohs.StringArray(Array(x.length),x)



// extension (i: Int)
//   def pick(index: Int): Int = i
//   def +(arr: Mohs[Int]) = arr.map(n => n + i)

@main def hello(): Unit =
  // println(Mohs.flat(Iota(25).reshape(Array(5,5)).indexes))
  // println(Mohs.Linear(10, 20, 15))
  val array = (Mohs.Range(9, 12) table Mohs.Range(2, 7)).map(_ % _)
  val array2 = Mohs.List(100, 200, 300)
  val array3 = Mohs.List("Hi", "I", "Am")
  val array4 = Mohs.List(100, 200, 300)
  val array5 = Mohs.String("mississippi")
  val array6 = Mohs.List(1, 2, 3)
  val array3d = Mohs.List(1, 2, 3, 0, 5, 9, 3, 7).reshape(2,3,5)
  
  // prettyPrint(Mohs.concat(2)(List(
  //   Mohs.List(1, 2, 3, 10, 10, 20, 30, 100).reshape(2,4),
  //   Mohs.List(4, 5, 6, 20).reshape(2,2),
  //   Mohs.List(7, 8, 9, 30).reshape(2,2),
  // )))

  // prettyPrint(
  //   "aaaabbbbcccc111122223333".reshape(2,3,4).reorderAxis(1,0,2)
  // )

  prettyPrint(array3d)
  prettyPrint(array3d.windows(2, 2))

  // prettyPrint(array3d.axis(3)(Array(true, false, true, true)))


  // prettyPrint(array.axis(1).fold(_+_))
  // println(array.nonZero)
  // prettyPrint(array(Mohs.List(0, 1, 0, 2)))
  // prettyPrint(array6.indexOf(array))

  // prettyPrint(Mohs.String("helloworld").reshape(2,5).find(Mohs.String("l")))