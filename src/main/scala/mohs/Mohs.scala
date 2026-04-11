package mohs

import scala.annotation.static
import scala.annotation.targetName
import scala.specialized
import scala.reflect.ClassTag
import scala.collection.mutable

def windowsData(window: Array[Int], shape: Array[Int]): Array[Int] =
  val outShape = shape.zip(window).map(_-_+1) ++ window ++ shape.drop(window.length)
  require(outShape.forall(_>=0), "Invalid window size")
  return outShape

trait Mohs[@specialized(Int, Double, Float, Long, Char, Boolean) T:ClassTag]:
  val shape: Array[Int]
  def values: Array[T]

  def rank = shape.size
  def length = shape.head
  lazy val strides = shape.indices.map { i => shape.drop(i + 1).product }

  def at(x: Int*): T = 
    require(x.length == rank)
    if !(x.zip(shape).forall(_ < _)) then throw IndexOutOfBoundsException(x.mkString("(",",",")"))
    val index = strides.zip(x).map(_*_).sum
    flat_(index)

  def at(x: Mohs[Int]): T = at(x.values*)

  def flat_(x: Int): T

  /* === AXIS METHODS === */

  infix def ~(x: Int): MohsRanked[T] = axis(x)

  def axis(x: Int): MohsRanked[T] = 
    // require(x <= rank && x > 0, "Invalid axis")
    MohsRankedView(this, x)

  def tile(n: Int): Mohs[T] = 
    val count = shape.product
    val data = Stream.continually(values.toStream).flatten.take(count*n).toArray
    BaseArray(shape.prepended(n), data)

  def repeat(n: Int): Mohs[T] = this.axis(1).repeat(n)
  def repeat(n: Int*): Mohs[T] = this.axis(1).repeat(n*)

  def take(n: Int*): Mohs[T] =
    require(n.length <= rank)
    var result = this
    for (k, i) <- n.zipWithIndex do
      result = result.axis(i+1).take(k)
    result

  def drop(n: Int*): Mohs[T] =
    require(n.length <= rank)
    var result = this
    for (k, i) <- n.zipWithIndex do
      result = result.axis(i+1).drop(k)
    result


  def take(n: Int): Mohs[T] = this.axis(1).take(n)
  def drop(n: Int): Mohs[T] = this.axis(1).drop(n)
  def roll(n: Int): Mohs[T] = this.axis(1).roll(n)
  def padStart(n: Int, fill: T): Mohs[T] = this.axis(1).padStart(n, fill)
  def padEnd(n: Int, fill: T): Mohs[T] = this.axis(1).padEnd(n, fill)
  def reverse: Mohs[T] = this.axis(1).reverse

  /* === end AXIS METHODS === */

  // infix def withFill(fill: T): Mohs[T] = BaseArray(this.shape, this.values, fill)


  def reshape(sh: Array[Int]): Mohs[T] =
    val count = sh.product
    val data = Stream.continually(values.toStream).flatten.take(count).toArray
    BaseArray(sh, data)
  
  def reshape(sh: Int*): Mohs[T] = reshape(sh.toArray)

  def ravel: Mohs[T] =
    BaseArray(Array(shape.product), values)

  /**
   * Moves the first axis to the end
   */
  def transpose: Mohs[T] =
    require(rank > 1, "Invalid rank")
    val shape1 = shape.tail appended length
    val data = cells(1).toArray
      .map ( _.values )
      .transpose
      .flatten

    BaseArray(shape1, data)

  def reorderAxis(order: Int*): Mohs[T] =
    require(order.length == rank)
    val newShape = Array.ofDim[Int](rank)
    for (a,i) <- shape.zipWithIndex do
      newShape(order(i)) = a

    Indexes(newShape).map(i => this.at(i(order)))

  def groupBy[K](keys: Mohs[Int]): Array[Mohs[T]] =
    val indices = Mohs.groupIndices(keys.values)
    indices.values.map(this(_))


  def windows(window: Array[Int]): Mohs[T] =
    require(window.length<=this.rank, "window rank must be less or equal array rank")
    Windows(window, this)

  def windows(window: Int*): Mohs[T] =
    require(window.length<=this.rank, "window rank must be less or equal array rank")
    Windows(window.toArray, this)

  def find(pattern: Mohs[T]): Mohs[Boolean] =
    require(pattern.shape.length<=this.rank, "pattern rank must be less or equal array rank")
    val windowShape = pattern.shape.reverse.padTo(rank, 1).reverse
    val patternValues = pattern.values
    val w = windows(windowShape)
    val data = w.cells(rank).map {
      _.values.sameElements(patternValues)
    }
    BaseArray(w.shape.take(rank), data.toArray)

  def find(x: T): Mohs[Boolean] =
    val data = values.map { y => x == y }
    BaseArray(shape, data)

  

  def cells(n: Int): Iterator[Mohs[T]] =
    val count = shape.take(n).product
    Iterator.tabulate(count)(CellView(this, n, _))

  def zip[U](that: Mohs[U]): Mohs[(T,U)] = Zip(this, that)
  def zipWithIndex[U]: Mohs[(T,Mohs[Int])] = Zip(this, Indexes(shape))
  def map[U:ClassTag](fn: (T) => U): Mohs[U] = new Map(this, fn)
  
  def reduce(fn: (T, T) => T): T =
    require(rank == 1, "Invalid rank")
    values.reduce(fn)

  def scan(z: T)(fn: (T, T) => T): Mohs[T] =
    require(rank == 1, "Invalid rank")
    val data = values.scan(z)(fn)
    BaseArray(Array(length+1), data)

  infix def table[U](that: Mohs[U]): Mohs[(T, U)] =
    val shape = this.shape concat that.shape
    val data = for 
      i <- this.values
      j <- that.values
    yield (i, j)
    BaseArray(shape, data)

  def table[U,V:ClassTag](fn: (T, U) => V)(that: Mohs[U]): Mohs[V] =
    val shape = this.shape concat that.shape
    val data = for 
      i <- this.values
      j <- that.values
    yield fn(i, j)
    BaseArray(shape, data)
    

  def ==(that: Mohs[T]): Mohs[Boolean] = zip(that).map(_ == _)
  def ==(that: T): Mohs[Boolean] = map(_ == that)
  
  def equals(that: Mohs[T]): Boolean = 
    if !that.shape.zip(this.shape).forall(_.equals(_)) then return false
    return that.values.zip(this.values).forall(_.equals(_))

  def indexes: Mohs[Mohs[Int]] = Indexes(shape)

  def classify(): Mohs[Int] = mohs.classify(this)
  def occurrenceCount(): Mohs[Int] = mohs.occurrenceCount(this)
  def markFirsts(): Mohs[Boolean] = occurrenceCount()==MohsInt(0)
  def deduplicate(): Mohs[T] = this(markFirsts())

  def isIn(x: Iterable[T]): Mohs[Boolean] = mohs.isIn(this, x)
  def isIn(that: Mohs[T]): Mohs[Boolean] = mohs.isIn(this, that)
  def indexOf(that: Mohs[T]): Mohs[Int] = mohs.indexOf(that, this)

  def nonZero =
    this.indexes.values.zip(this.values).flatMap { 
      case (_, false) => None
      case (_, 0) => None
      case (_, 0.0) => None
      case (_, "") => None
      case (index, _) => Some(index)
    }


  def ++(that: Mohs[T]) =
    val len = this.rank - that.rank match
      case 0 => 
        require(this.shape.tail sameElements that.shape.tail, "Major cells must have the same shape")
        length+that.length
      case 1 =>
        require(this.shape.tail sameElements that.shape, "Major cells must have the same shape")
        length+1
      case -1 =>
        require(this.shape sameElements that.shape.tail, "Major cells must have the same shape")
        that.length+1
      case _ => 
        throw IllegalArgumentException("Ranks of arrays must be the same or one apart")

    BaseArray(this.shape.updated(0, len), this.values ++ that.values)
  
  def ++(that: T) =
    if rank == 1 then
      val data = this.values.appended(that)
      BaseArray(this.shape.tail.prepended(length+1), data)
    else
      throw IllegalArgumentException("Ranks of arrays must be the same or one apart")
    

  @targetName("apply_bool") def apply(mask: Mohs[Boolean]) = 
    val cells = this.cells(mask.rank)
    Mohs.merge(mask.values.zip(cells).filter(_._1).map(_._2))

  @targetName("apply_int") def apply(x: Mohs[Int]) = 
    val cells = this.cells(1).toArray
    val data = x.values.flatMap(cells(_).values).toArray
    val shape = x.shape ++ this.shape.drop(1)
    BaseArray(shape, data)

  def apply(x: Iterable[Int]) = 
    val cells = this.cells(1).toArray
    val data = x.flatMap(cells(_).values).toArray
    val shape = this.shape.updated(0, x.size)
    BaseArray(shape, data)

  def collect[V:ClassTag](pf: PartialFunction[T,V]): Mohs[V] =
    val cells = values.collect(pf)
    BaseArray(Array(cells.length), cells.toArray)

  override def toString(): String = display[T](this)



  def >(that: Mohs[T])(implicit num: Ordering[T]): Mohs[Boolean] = zip(that).map(num.gt)
  def <(that: Mohs[T])(implicit num: Ordering[T]): Mohs[Boolean] = zip(that).map(num.lt)
  def >=(that: Mohs[T])(implicit num: Ordering[T]): Mohs[Boolean] = zip(that).map(num.gteq)
  def <=(that: Mohs[T])(implicit num: Ordering[T]): Mohs[Boolean] = zip(that).map(num.lteq)
  def >(that: T)(implicit num: Ordering[T]): Mohs[Boolean] = map(num.gt(_,that))
  def <(that: T)(implicit num: Ordering[T]): Mohs[Boolean] = map(num.lt(_,that))
  def >=(that: T)(implicit num: Ordering[T]): Mohs[Boolean] = map(num.gteq(_,that))
  def <=(that: T)(implicit num: Ordering[T]): Mohs[Boolean] = map(num.lteq(_,that))


  def binop(fn: (T,T) => T)(that: Mohs[T]) = zip(that).map(fn.tupled)
  def binopScalar(fn: (T,T) => T)(that: T) = map(fn(_, that))

  def +(that: Mohs[T])(implicit num: Numeric[T]): Mohs[T] = binop(num.plus)(that)
  def -(that: Mohs[T])(implicit num: Numeric[T]): Mohs[T] = binop(num.minus)(that)
  def *(that: Mohs[T])(implicit num: Numeric[T]): Mohs[T] = binop(num.times)(that)
  def /(that: Mohs[T])(implicit num: Numeric[T]): Mohs[Double] = zip(that).map(num.toDouble(_:T)/num.toDouble(_:T))
  def **(that: Mohs[T])(implicit num: Numeric[T]): Mohs[Double] = 
    zip(that).map((a,b) => scala.math.pow(num.toDouble(a), num.toDouble(b)))
  def %(that: Mohs[T])(implicit num: Numeric[T]): Mohs[Int] = zip(that).map(num.toInt(_:T)%num.toInt(_:T))
  infix def maximum(that: Mohs[T])(implicit num: Numeric[T]): Mohs[T] = binop(num.max)(that)
  infix def minimum(that: Mohs[T])(implicit num: Numeric[T]): Mohs[T] = binop(num.min)(that)

  def +(that: T)(implicit num: Numeric[T]): Mohs[T] = binopScalar(num.plus)(that)
  def -(that: T)(implicit num: Numeric[T]): Mohs[T] = binopScalar(num.minus)(that)
  def *(that: T)(implicit num: Numeric[T]): Mohs[T] = binopScalar(num.times)(that)
  def /(that: T)(implicit num: Numeric[T]): Mohs[Double] = map(num.toDouble(_:T)/num.toDouble(that))
  def **(that: T)(implicit num: Numeric[T]): Mohs[Double] = 
    map(a => scala.math.pow(num.toDouble(a), num.toDouble(that)))
  def %(that: T)(implicit num: Numeric[T]): Mohs[Int] = map(num.toInt(_:T)%num.toInt(that))
  infix def maximum(that: T)(implicit num: Numeric[T]): Mohs[T] = binopScalar(num.max)(that)
  infix def minimum(that: T)(implicit num: Numeric[T]): Mohs[T] = binopScalar(num.min)(that)

  def unary_-(implicit num: Numeric[T]): Mohs[T] = UFunc.neg(this)
  def abs(implicit num: Numeric[T]): Mohs[T] = UFunc.abs(this)
  def sign(implicit num: Numeric[T]): Mohs[T] = UFunc.sign(this)

  def sum(implicit num: Numeric[T]) = UFunc.sum(this)
  def max(implicit num: Numeric[T]) = UFunc.max(this)
  def min(implicit num: Numeric[T]) = UFunc.min(this)
  def argmin(implicit num: Numeric[T]) = UFunc.argmin(this)
  def argmax(implicit num: Numeric[T]) = UFunc.argmax(this)

  def sum(axis: Int)(implicit num: Numeric[T]) = UFunc.sum(this.axis(axis))
  def max(axis: Int)(implicit num: Numeric[T]) = UFunc.max(this.axis(axis))
  def min(axis: Int)(implicit num: Numeric[T]) = UFunc.min(this.axis(axis))
  def mean(axis: Int)(implicit num: Numeric[T]) = UFunc.mean(this.axis(axis))
  def argmin(axis: Int)(implicit num: Numeric[T]) = UFunc.argmin(this.axis(axis))
  def argmax(axis: Int)(implicit num: Numeric[T]) = UFunc.argmax(this.axis(axis))
  


object Mohs extends MohsNumericOps:
  // @targetName("list_i") def List[T](s: Int*): Mohs[Int] = BaseArray(Array(s.length), s.toList)
  // @targetName("list_f") def List[T](s: Float*): Mohs[Float] = BaseArray(Array(s.length), s.toList)
  // @targetName("list_d") def List[T](s: Double*): Mohs[Double] = BaseArray(Array(s.length), s.toList)
  // @targetName("list_l") def List[T](s: Long*): Mohs[Long] = BaseArray(Array(s.length), s.toList)
  // @targetName("list_str") def List[T](s: String*): Mohs[String] = BaseArray(Array(s.length), s.toList)
  // @targetName("list_b") def List[T](s: Boolean*): Mohs[Boolean] = BaseArray(Array(s.length), s.toList)
  @targetName("list_gen") def List[T:ClassTag](s: T*): Mohs[T] = BaseArray(Array(s.length), s.toArray)
  @targetName("list_iter") def List[T:ClassTag](s: Iterable[T]) = BaseArray(Array(s.size), s.toArray)
  
  def apply[T:ClassTag](sh: Array[Int], data: Array[T]): Mohs[T] =
    BaseArray(sh, data)

  def FromFn[T:ClassTag](s: (Mohs[Int] => T), shape: Array[Int]) =
    Indexes(shape).map(s)

  def String(s: String) = StringArray(Array(s.length), s)
  
  def Fill[@specialized(Int, Double, Float, Long) T:ClassTag](fill: T)(sh: Int*): Mohs[T] = 
    BaseArray(sh.toArray, Array.fill[T](sh.product)(fill))

  def Iota(i: Int) = RangeArray(0, i)
  def Range(start: Int, end: Int) = RangeArray(start, end)
  def Range(start: Int, end: Int, step: Int) = RangeArray(start, end, step)

  def Linear(start: Double, end: Double, q: Int) = 
    List((0 to q).map(x => (x.toFloat / q) * (end-start) + start)*)

  def flat[T:ClassTag](a: Mohs[Mohs[T]]): Mohs[T] =
    val shape_assert = a.values.map { _.shape }
    assert(shape_assert.forall(shape_assert.head.sameElements))

    val new_shape = a.shape ++ shape_assert.head
    val values = a.values
    BaseArray(new_shape, values.flatMap(_.values))

  def merge[T:ClassTag](arrays: Seq[Mohs[T]]) =
    require(arrays.tail.forall(_.shape.sameElements(arrays.head.shape)))
    val data = arrays.flatMap(_.values)
    val shape = arrays.head.shape.prepended(arrays.length)
    BaseArray(shape, data.toArray)

  def unwrap[T:ClassTag,U:ClassTag](array: Mohs[(T,U)]): (Mohs[T], Mohs[U]) =
    (array.map(_._1), array.map(_._2))

  def where[T:ClassTag](array: Mohs[Boolean], x: Mohs[T], y: Mohs[T]) =
    val x_ = Zip(array, x)
    val y_ = Zip(array, y)

    val data = array.values.zipWithIndex.map((cond, i) => if cond then x_.flat_(i)._2 else y_.flat_(i)._2)
    BaseArray(array.shape, data)
    // array.map(if _ then )

  def stack[T:ClassTag](axis: Int)(arrays: Iterable[Mohs[T]]) =
    require(arrays.tail.forall(_.shape.sameElements(arrays.head.shape)))
    require(axis < arrays.head.rank && axis > 0)
    if axis == 1 then merge(arrays.toList)
    else
      val (shapeA, shapeB) = arrays.head.shape.splitAt(axis)
      val shape = (shapeA appended arrays.size) ++ shapeB
      val data = arrays
        .map { _.cells(axis).toVector }
        .transpose
        .map { x => x.flatMap(_.values) }
        .flatten

      BaseArray(shape, data.toArray)

  def concat[T:ClassTag](axis: Int)(arrays: Iterable[Mohs[T]]) =
    require(axis <= arrays.head.rank && axis > 0, "Invalid axis")
    var size = 0
    val checkShape = arrays.head.shape.updated(axis-1, 0)
    for arr <- arrays do
      size += arr.shape(axis-1)
      val changedShape = arr.shape.updated(axis-1, 0)
      require(changedShape sameElements checkShape)

    val finalShape = arrays.head.shape.updated(axis-1, size)
    
    if axis == 1 then
      val data = arrays.flatMap(_.values)
      BaseArray(finalShape, data.toArray)
    else
      val data = arrays
        .map { _.cells(axis-1).toVector }
        .transpose
        .map { x => x.flatMap(_.values) }
        .flatten

      BaseArray(finalShape, data.toArray)

  def stack[T:ClassTag](axis: Int)(array: Mohs[T]) =
    require(axis < array.rank && axis > 0, "Invalid axis")
    if axis == 1 then array
    else
      val (shapeA, shapeB) = array.shape.tail.splitAt(axis)
      val shape = (shapeA appended array.length) ++ shapeB
      val data = array.cells(1).toArray
        .map { _.cells(axis).toArray }
        .transpose
        .map { x => x.flatMap(_.values) }
        .flatten

      BaseArray(shape, data)

  def groupIndices(keys: Seq[Int]): Mohs[Mohs[Int]] =
    val max = keys.max
    val array = Array.fill(max+1)(scala.List[Int]())
    for (k,i) <- keys.zipWithIndex do
      if k >= 0 then array(k) = array(k).appended(i)

    List(array.map(List).toIterable)

  def Random(sh: Int*): Mohs[Double] =
    val rand = new scala.util.Random

    if (sh.length == 0) return MohsDouble(rand.nextGaussian())

    val data = Array.fill(sh.product)(rand.nextGaussian())
    BaseArray(sh.toArray, data)


  def -[T:ClassTag](tuple: (Mohs[T], Mohs[T]))(implicit num: Numeric[T]): Mohs[T] = 
    tuple._1.zip(tuple._2).map(num.minus)
    
class StringArray(val shape: Array[Int], private val data: String) extends Mohs[Char] {
  def values = data.toCharArray()

  def flat_(x: Int) = data.charAt(x)
}

class Zip[T,U](private val a: Mohs[T], private val b: Mohs[U]) extends Mohs[(T,U)] {
  val stride = conform(a, b)
  require(stride != None, f"Shapes must match: (${a.shape.mkString(",")}) zip (${b.shape.mkString(",")})")

  val shape = if a.rank > b.rank then a.shape else b.shape
  def values: Array[(T, U)] =
    val r = stride.get
    if r > 0 then
      val other = b.values
      a.values.zipWithIndex.map((t, i) => (t, other(i/r)))
    else
      val other = a.values
      b.values.zipWithIndex.map((t, i) => (other(i/(-r)), t))

  def flat_(i: Int) =
    val r = stride.get
    if r > 0 
    then (a.flat_(i), b.flat_(i/r))
    else (a.flat_(i/(-r)), b.flat_(i))
}

class Map[T,U:ClassTag](private val a: Mohs[T], private val fn: (T) => U) extends Mohs[U] {
  val shape = a.shape
  def values = a.values.map(fn)
  // def pick(index: Int) = fn(a.pick(index))
  override def map[V:ClassTag](fn2: U => V) = new Map(a, fn.andThen(fn2))

  def flat_(i: Int) = fn(a.flat_(i))
}

class Box[T:ClassTag](x: T) extends Mohs[T] {
  val shape = Array[Int]()
  def values = Array(x)
  def flat_(i: Int) = x
}

class MohsInt(n: Int) extends Mohs[Int] {
  val shape = Array[Int]()
  def values: Array[Int] = Array(n)
  def asInt: Int = n
  def flat_(x: Int): Int = n
}

class MohsDouble(n: Double) extends Mohs[Double] {
  val shape = Array[Int]()
  def values: Array[Double] = Array(n)
  def asDouble: Double = n
  def flat_(x: Int): Double = n
}

class BaseArray[@specialized(Int, Double, Float, Long, Char, Boolean) T:ClassTag](private val sh: Array[Int], private val data: Array[T]) extends Mohs[T] {
  assert(sh.length == 0 || sh.product == data.length, f"Invalid array: shape (${sh.mkString(",")}) and data size ${data.size} don't match")
  
  val shape = sh
  def values = data

  def flat_(i: Int): T = data(i)
}

class RangeArray(i: Int, j: Int, step: Int=1) extends Mohs[Int] {
  val shape = Array((j-i) / step)
  def values = Array.range(i, j, step)
  
  def flat_(k: Int) = i+step*k

  infix def by(step: Int) = RangeArray(i, j, step)
}

class Indexes(private val sh: Array[Int]) extends Mohs[Mohs[Int]] {
  val shape = sh
  def values = Array.tabulate(shape.product)(represent(sh))

  def flat_(x: Int) = represent(sh)(x)
}

class Windows[T:ClassTag](private val window: Array[Int], private val a: Mohs[T]) extends Mohs[T] {
  private val shape_ = windowsData(window, a.shape)
  val shape = shape_
  def values = Array.tabulate(shape.product)(flat_)

  def flat_(x: Int): T =
    val l = window.length
    val r = Array.fill(a.shape.length)(0)
    var k = x
    var s = shape.length
    var stride = 1
    while s > 0 do
      s -= 1

      val j = shape(s)
      if s < l then
        r(s) += (if j == 0 then k else k % j)
        r(s) *= a.strides(s)
      else
        r(s-l) += (if j == 0 then k else k % j)
        if s >= l*2 then
          r(s-l) *= a.strides(s-l)
      
      k = if j == 0 then 0 else k / j
      
    // val index = r.zip(a.strides).map(_*_).sum

    // println(r.mkString(";"))
    // a.at(r(0), r(2))
    a.flat_(r.sum)
    // val base = x % bases_.length
    // val index = x / bases_.length
    // a.flat_(bases_(base)+indexes_(index))

}

class CellView[T:ClassTag](private val a: Mohs[T], private val _rank: Int, private val cell: Int) extends Mohs[T] {
  val shape = a.shape.drop(_rank)
  val cellOffset = shape.product*cell
  def values = Array.tabulate(shape.product)(flat_)

  def flat_(x: Int) = a.flat_(cellOffset+x)
}

def _leadingAxisExtension(s: Array[Int], t: Array[Int]): Option[Int] =
  var n = 1
  var m = 0
  while m < t.size do
    if s(m) != t(m) then return None
    m += 1
  while m < s.size do
    n *= s(m)
    m += 1
  return Some(n)

def conform(a: Mohs[?], b: Mohs[?]) =
  if a.rank >= b.rank then _leadingAxisExtension(a.shape, b.shape)
  else _leadingAxisExtension(b.shape, a.shape).map(-_)

def represent(shape: Array[Int])(index: Int): Mohs[Int] =
  val r = Array.fill(shape.length)(1)
  var k = index
  var s = shape.length
  while s > 0 do
    s -= 1
    val j = shape(s)
    r(s) = if j == 0 then k else k % j
    k = if j == 0 then 0 else k / j

  BaseArray(Array(shape.length), r)

def representAxis(shape: Array[Int], index: Int, axis: Int): Int =
  var r = 0
  var k = index/shape(axis)
  var stride = 1
  var s = shape.length
  while s > 0 do
    s -= 1
    val j = shape(s)
    if j == 0 then
      s = 0
    else 
      if s == axis then
        r += (index % j)*stride
        k = k / 1
      else
        r += (k % j)*stride
        k = k / j
      stride *= j

  r