package mohs

trait MohsLike[T]:
    def shape: Array[Int]
    def values: List[T]

trait MohsListLike[A, T]:
    def likeList(a: A): List[T]

given [T]: MohsListLike[Mohs[T], T] with
    def likeList(a: Mohs[T]) = 
        require(a.rank == 1)
        a.values.toList

given [T]: MohsListLike[Array[T], T] with
        def likeList(a: Array[T]) =  a.toList
given [T]: MohsListLike[Seq[T], T] with
    def likeList(a: Seq[T]) = a.toList
given [T]: MohsListLike[List[T], T] with
    def likeList(a: List[T]) = a

