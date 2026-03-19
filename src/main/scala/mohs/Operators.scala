package mohs

import scala.reflect.ClassTag

trait MohsNumericOps:
  extension [T:ClassTag](self: Mohs[T])
    def +(that: Mohs[T])(implicit num: Numeric[T]): Mohs[T] = self.zip(that).map(num.plus)
    def -(that: Mohs[T])(implicit num: Numeric[T]): Mohs[T] = self.zip(that).map(num.minus)
    def unary_-(implicit num: Numeric[T]): Mohs[T] = self.map(num.negate)
    def *(that: Mohs[T])(implicit num: Numeric[T]): Mohs[T] = self.zip(that).map(num.times)
    def /(that: Mohs[T])(implicit num: Fractional[T]): Mohs[T] = self.zip(that).map(num.div)
    def /(that: Mohs[T])(implicit num: Integral[T]): Mohs[T] = self.zip(that).map(num.quot)
    def %(that: Mohs[T])(implicit num: Integral[T]): Mohs[T] = self.zip(that).map(num.rem)
    def abs(implicit num: Numeric[T]): Mohs[T] = self.map(num.abs)
    def sign(implicit num: Numeric[T]): Mohs[T] = self.map(num.sign)
    def max(that: Mohs[T])(implicit num: Ordering[T]): Mohs[T] = self.zip(that).map(num.max)
    def min(that: Mohs[T])(implicit num: Ordering[T]): Mohs[T] = self.zip(that).map(num.min)

    def +(that: T)(implicit num: Numeric[T]): Mohs[T] = self.map(num.plus(_,that))
    def -(that: T)(implicit num: Numeric[T]): Mohs[T] = self.map(num.minus(_,that))
    def *(that: T)(implicit num: Numeric[T]): Mohs[T] = self.map(num.times(_,that))
    
    // def /(that: T)(implicit num: Fractional[T]): Mohs[T] = self.map(num.div(_,that))
    // def /(that: T)(implicit num: Integral[T]): Mohs[T] = self.map(num.quot(_,that))
    def /(that: Double)(implicit num: Numeric[T]): Mohs[Double] = self.map(i => num.toDouble(i)/that)
    def %(that: T)(implicit num: Integral[T]): Mohs[T] = self.map(num.rem(_,that))
    def max(that: T)(implicit num: Ordering[T]): Mohs[T] = self.map(num.max(_,that))
    def min(that: T)(implicit num: Ordering[T]): Mohs[T] = self.map(num.min(_,that))

    def toDouble(implicit num: Numeric[T]): Mohs[Double] = self.map(num.toDouble)
    def toInt(implicit num: Numeric[T]): Mohs[Int] = self.map(num.toInt)
    def toFloat(implicit num: Numeric[T]): Mohs[Float] = self.map(num.toFloat)
    def toLong(implicit num: Numeric[T]): Mohs[Long] = self.map(num.toLong)

    def sum(implicit num: Numeric[T]): T = self.values.sum(using num)

  extension [T:ClassTag](self: Mohs[Boolean])

    def toDouble: Mohs[Double] = self.map(if (_) 1 else 0)
    def toInt: Mohs[Int] = self.map(if (_) 1 else 0)
    def toFloat: Mohs[Float] = self.map(if (_) 1 else 0)
    def toLong: Mohs[Long] = self.map(if (_) 1 else 0)

