package mohs

def classify[T](array: Mohs[T]): Mohs[Int] =
  val cells = array.cells(1).toArray
  val data = Array.ofDim[Int](cells.length)
  var counter = 0
  for (c, i) <- cells.zipWithIndex do
    var j = i-1
    while j >= 0 do
      if cells(j).equals(c) then
        data(i) = data(j)
        j = -2
      else
        j -= 1

    if j == -1 then
      data(i) = counter
      counter += 1
  BaseArray(Array(cells.length), data)

def occurrenceCount[T](array: Mohs[T]): Mohs[Int] =
  val cells = array.cells(1).toArray
  val data = Array.ofDim[Int](cells.length)
  for (c, i) <- cells.zipWithIndex do
    var counter = 0
    var j = i-1
    while j >= 0 do
      if cells(j).equals(c) then
        counter += 1
      j -= 1

    data(i) = counter
  BaseArray(Array(cells.length), data)

def isIn[T](searchFor: Mohs[T], x: Iterable[T]): Mohs[Boolean] =
  val searchIn = x.toSet
  val data = searchFor.values.map { y =>
    searchIn.contains(y)
  }
  BaseArray(searchFor.shape, data)

def isIn[T](searchFor: Mohs[T], searchIn: Mohs[T]): Mohs[Boolean] =
  val rank = searchFor.rank-searchIn.rank+1
  val searchForCells = searchFor.cells(rank)
  val searchInCells = searchIn.cells(1)
  val data = searchForCells.map { y =>
    searchInCells.exists(z => z.equals(y))
  }
  BaseArray(searchFor.shape.take(rank), data.toArray)

def indexOf[T](searchFor: Mohs[T], searchIn: Mohs[T]): Mohs[Int] =
  val rank = searchFor.rank-searchIn.rank+1
  val searchForCells = searchFor.cells(rank)
  val searchInCells = searchIn.cells(1)
  val data = searchForCells.map { y =>
    val x = searchInCells.indexWhere(z => z.equals(y))
    if x == -1 then searchInCells.length else x
  }
  BaseArray(searchFor.shape.take(rank), data.toArray)