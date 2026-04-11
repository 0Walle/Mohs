package mohs

import scala.reflect.ClassTag
import scala.annotation.targetName

trait MohsNumericOps:
  extension [T:ClassTag](self: Mohs[T])(using num: Numeric[T])
    @targetName("matmul")
    infix def <*>(that: Mohs[T]): Mohs[T] =
      (self~1).flatMap(x => (x * that).sum(1))

    infix def dot(that: Mohs[T]): Mohs[Double] =
      if self.rank == 1 then return self.reshape(Array(1, self.shape*)).dot(that)

      if that.rank == 1 then 
        val data = self.cells(1).map{x=>
          num.toDouble(UFunc.sum(x*that))
        }
        BaseArray(Array(self.length), data.toArray)
      else
        ???
      /* [a, b, c] dot [d, e, f]
         = [a*d+b*e+c*f]

         [[a, b, c]...] dot [d, e, f]
         = [a*d+b*e+c*f,]

         [[a, b, c]...] dot [[d, e, f]...]
         = [a*d+b*e+c*f, ...]



       */    

object UFunc {
  def sum[T:ClassTag](self: Mohs[T])(implicit num: Numeric[T]): T = self.values.sum
  def sum[T:ClassTag](self: MohsRanked[T])(implicit num: Numeric[T]): Mohs[T] = self.agg(sum)
  def max[T:ClassTag](self: Mohs[T])(implicit num: Numeric[T]): T = self.values.max
  def max[T:ClassTag](self: MohsRanked[T])(implicit num: Numeric[T]): Mohs[T] = self.agg(max)
  def min[T:ClassTag](self: Mohs[T])(implicit num: Numeric[T]): T = self.values.min
  def min[T:ClassTag](self: MohsRanked[T])(implicit num: Numeric[T]): Mohs[T] = self.agg(min)

  def argmin[T:ClassTag](self: Mohs[T])(implicit num: Numeric[T]): Int = self.values.zipWithIndex.minBy(_._1)._2
  def argmin[T:ClassTag](self: MohsRanked[T])(implicit num: Numeric[T]): Mohs[Int] = self.agg(argmin)
  def argmax[T:ClassTag](self: Mohs[T])(implicit num: Numeric[T]): Int = self.values.zipWithIndex.maxBy(_._1)._2
  def argmax[T:ClassTag](self: MohsRanked[T])(implicit num: Numeric[T]): Mohs[Int] = self.agg(argmax)
  
  def mean[T:ClassTag](self: Mohs[T])(implicit num: Numeric[T]): Double = num.toDouble(sum(self)) / self.length
  def mean[T:ClassTag](self: MohsRanked[T])(implicit num: Numeric[T]): Mohs[Double] = self.agg(mean)

  def toDouble(self: Mohs[Boolean]): Mohs[Double] = self.map(if (_) 1 else 0)
  def toInt(self: Mohs[Boolean]): Mohs[Int] = self.map(if (_) 1 else 0)
  def toFloat(self: Mohs[Boolean]): Mohs[Float] = self.map(if (_) 1 else 0)
  def toLong(self: Mohs[Boolean]): Mohs[Long] = self.map(if (_) 1 else 0)
  def toDouble[T](self: Mohs[T])(implicit num: Numeric[T]): Mohs[Double] = self.map(num.toDouble)
  def toInt[T](self: Mohs[T])(implicit num: Numeric[T]): Mohs[Int] = self.map(num.toInt)
  def toFloat[T](self: Mohs[T])(implicit num: Numeric[T]): Mohs[Float] = self.map(num.toFloat)
  def toLong[T](self: Mohs[T])(implicit num: Numeric[T]): Mohs[Long] = self.map(num.toLong)

  def sqrt[T:ClassTag](self: Mohs[T])(implicit num: Numeric[T]) =
    self.map(x => scala.math.sqrt(num.toDouble(x)))
  def sqrt[T:ClassTag](self: T)(implicit num: Numeric[T]) =
    scala.math.sqrt(num.toDouble(self))

  def neg[T:ClassTag](self: Mohs[T])(implicit num: Numeric[T]) = self.map(num.negate)
  def neg[T:ClassTag](self: T)(implicit num: Numeric[T]) = num.negate
  def abs[T:ClassTag](self: Mohs[T])(implicit num: Numeric[T]) = self.map(num.abs)
  def abs[T:ClassTag](self: T)(implicit num: Numeric[T]) = num.abs
  def sign[T:ClassTag](self: Mohs[T])(implicit num: Numeric[T]) = self.map(num.sign)
  def sign[T:ClassTag](self: T)(implicit num: Numeric[T]) = num.sign
}