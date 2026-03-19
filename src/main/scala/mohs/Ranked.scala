package mohs

import scala.reflect.ClassTag

private def mergeUnsafe_[T:ClassTag](arrays: Iterator[Mohs[T]], shape: Array[Int]) =
    val data = arrays.flatMap(_.values)
    // val shape = arrays.head.shape.prepended(arrays.length)
    BaseArray(shape, data.toArray)

trait MohsRanked[T:ClassTag]:
    val axis: Int
    val ref: Mohs[T]

    def majorCells: Iterator[Mohs[T]] = ref.cells(axis)

    def cells: List[Mohs[T]] =
        val originalData = ref.values
        val count = ref.shape(axis-1)
        val data = (0 until ref.shape.product by count).map{ i =>
            Mohs.List((0 until count).map(j => originalData(representAxis(ref.shape, i+j, axis-1))))
        }
        return data.toList

    def repeat(reps: Int): Mohs[T] =
        val shape = ref.shape.updated(axis-1, ref.shape(axis-1)*reps)
        Mohs.merge(majorCells.flatMap(x => Iterator.fill(reps)(x)).toList).reshape(shape)

    def repeat(reps: Int*): Mohs[T] =
        require(reps.length == majorCells.length)
        Mohs.merge(majorCells.zip(reps).flatMap((x, n) => List.fill(n)(x)).toList)


    def take(n: Int): Mohs[T] =
        val mod = ref.shape(axis-1)
        val data = for (j, i) <- majorCells.zipWithIndex if i % mod < n yield j
        val shape = ref.shape.updated(axis-1, n.min(mod))
        mergeUnsafe_(data, shape)

    def drop(n: Int): Mohs[T] =
        val mod = ref.shape(axis-1)
        val data = for (j, i) <- majorCells.zipWithIndex if i % mod >= n yield j
        val shape = ref.shape.updated(axis-1, n.min(mod))
        mergeUnsafe_(data, shape)

    def reverse: Mohs[T] =
        val mod = ref.shape(axis-1)
        val cells = majorCells.toArray
        val data = for i <- (0 until cells.length).iterator yield
            val greater = (i/mod+1)*mod
            val minor = i%mod
            cells(greater-minor-1)
        mergeUnsafe_(data, ref.shape)

    def roll(_n: Int): Mohs[T] =
        var n = _n
        val mod = ref.shape(axis-1)
        if n < 0 then n = mod + n
        val cells = majorCells.toArray
        val data = for i <- 0 until cells.length yield
            val greater = (i/mod)*mod
            val shift = (i%mod+n)%mod
            cells(greater+shift)
        mergeUnsafe_(data.iterator, ref.shape)

    def padStart(n: Int, fill: T): Mohs[T] =
        val mod = ref.shape(axis-1)+n
        val shape = ref.shape.updated(axis-1, mod)
        val fillAxis = (Mohs.Iota(ref.rank) == axis-1).toInt * n
        Indexes(shape).map(i => 
            if i.at(axis-1) < n then fill else ref.at(i-fillAxis)
            )

    def padEnd(n: Int, fill: T): Mohs[T] =
        val mod = ref.shape(axis-1)
        val shape = ref.shape.updated(axis-1, mod+n)
        // val fillAxis = (Mohs.Iota(ref.rank) == axis-1).toInt * n
        Indexes(shape).map(i =>
            if i.at(axis-1) >= mod then fill else ref.at(i)
            )

    def reduce(fn: (T, T) => T): Mohs[T] =
        val originalData = ref.values
        val count = ref.shape(axis-1)
        val shape = ref.shape.patch(axis-1,Nil,1)
        val data = (0 until ref.shape.product by count).map{ i =>
            (0 until count)
                .map(j => originalData(representAxis(ref.shape, i+j, axis-1)))
                .reduce(fn)
        }
        return BaseArray(shape, data.toArray)

    def apply[A](mask: A)(implicit ev: MohsListLike[A, Boolean]) = 
        val cells = this.majorCells
        val m = ev.likeList(mask).padTo(ref.shape(axis-1), false)
        require(m.length == ref.shape(axis-1))
        val data = Stream.continually(m.toStream).flatten.zip(cells).filter(_._1).map(_._2)
        val shape = ref.shape.updated(axis-1, m.count(x=>x))
        mergeUnsafe_(data.iterator, shape)

    def map[U:ClassTag](fn: Mohs[T] => U): Mohs[U] =
        val data = majorCells.map(fn).toArray
        val shape = ref.shape.take(axis)
        BaseArray(shape, data)

    def flatMap[U:ClassTag](fn: Mohs[T] => Mohs[U]): Mohs[U] =
        val arrays = majorCells.map(fn).toVector
        require(arrays.tail.forall(_.shape.sameElements(arrays.head.shape)))
        val data = arrays.flatMap(_.values)
        val shape = ref.shape.take(axis)++arrays.head.shape
        BaseArray(shape, data.toArray)

class MohsRankedView[T:ClassTag](val ref: Mohs[T], val axis: Int) extends MohsRanked[T] {

}
