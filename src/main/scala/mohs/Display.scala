package mohs

import scala.reflect.ClassTag

val df = java.text.DecimalFormat("0.##", java.text.DecimalFormatSymbols(java.util.Locale.US))

private def joinStringLines(s1: String, s2: String) =
  val l1 = s1.linesIterator.map(_.length).max
  val l2 = s2.linesIterator.map(_.length).max

  val joined = s1.linesIterator.zipAll(s2.linesIterator, "", "")
    .map { case (x1, x2) => s"${x1.padTo(l1, ' ')} ${x2.padTo(l2, ' ')}" }
    .mkString("\n")
  joined

def spacedCells(shape: Array[Int], cells: Iterator[String]) =
  val strides = shape.dropRight(1).scanRight(1)(_*_)
  val cells_ = cells.toArray
  (for (c, i) <- cells_.zipWithIndex yield
    if i == cells_.size-1 then
      c
    else
      val space = strides.count(j => i%j==(j-1))
      c + "\n"*space + " ").mkString("[","","]")

def display[A:ClassTag](value: Mohs[A]): String = implicitly[ClassTag[A]] match {
  case ClassTag.Boolean => value.shape.mkString("("," ",")") + value.values.map{j => if j.asInstanceOf[Boolean] then '1' else '0'}.mkString("[", " ", "]")
  case ClassTag.Double => value.shape.mkString("("," ",")") + value.values.map(d => df.format(d)).mkString("[",", ","]")
  case ClassTag.Char => value.shape.mkString("("," ",")") + value.values.mkString("\"", "", "\"")
  case _ => value.shape.mkString("("," ",")") + value.values.mkString("[",", ","]")
}

def displayRank2[A: ClassTag](value: Mohs[A]): String = implicitly[ClassTag[A]] match {
  case ClassTag.Char => value.shape.mkString("("," ",")") + value.cells(1).map { i =>
    i.values.mkString
  }.mkString("\n\"", "\n ", "\"\n")
  case ClassTag.Boolean => value.shape.mkString("("," ",")") + value.cells(1).map { i =>
    i.values.map{j => if j.asInstanceOf[Boolean] then '1' else '0'}.mkString(" ")
  }.mkString("\n[", "\n ", "]")
  case _ => value.shape.mkString("("," ",")") + value.cells(1).map { i =>
    i.values.mkString("",", ","")
  }.mkString("\n[", "\n ", "]")
    //value.shape.mkString("("," ",")") + value.values.mkString("[",", ","]")
}

def displayRankGreater2[A: ClassTag](value: Mohs[A]): String = implicitly[ClassTag[A]] match {
  case ClassTag.Char => value.shape.mkString("("," ",")") + value.cells(value.rank-2).map { i =>
    i.values.mkString
  }.mkString("\n\"", "\n ", "\"\n")
  case ClassTag.Boolean => value.shape.mkString("("," ",")\n") + spacedCells(value.shape, value.cells(value.rank-1).map { i =>
    i.values.map{j => if j.asInstanceOf[Boolean] then '1' else '0'}.mkString(" ")
  })
  case _ => value.shape.mkString("("," ",")\n") + spacedCells(value.shape, value.cells(value.rank-1).map { i =>
    i.values.mkString("",", ","")
  })
    //value.shape.mkString("("," ",")") + value.values.mkString("[",", ","]")
}

def prettyDisplay[A: ClassTag](value: Mohs[A]): String = value.rank match
  case 1 => display(value)
  case 2 => displayRank2(value)
  case _ => displayRankGreater2(value)
  // case _ => display(value)

def prettyPrint[A: ClassTag](x: Mohs[A]) = println(prettyDisplay(x))
def prettyPrint[A: ClassTag](x: List[Mohs[A]]) = 
  println(x.map(prettyDisplay).reduce(joinStringLines))
def prettyPrint[A: ClassTag](x: Iterable[A]) = 
  println(x.mkString("[",", ","]"))