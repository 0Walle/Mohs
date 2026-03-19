
## Array creation routines

### from shape

```scala
def Fill[T](fill: T)(sh: Int*)                    // np.full(sh, fill)
```

### from data

```scala
def List(i: T*)                                   // np.array([*i])
def List(i: Iterable[T], fill: T)                 // np.fromiter(i)
def FromFn(s: Mohs[Int] => T, shape: Array[Int])  // np.fromfunction(fn, shape)
```

### ranges

```scala
def Iota(i: Int)                                  // np.arange(i)
def Range(start: Int, end: Int)                   // np.arange(start, end)
def Range(start: Int, end: Int, step: Int)        // np.arange(start, end, step)
def Linear(start: Double, end: Double, q: Int)    // np.linspace(start, end, q)
```

## Change array shape

```scala
def ravel(): Mohs[T]                              // ndarray.ravel()
def reshape(sh: Int*): Mohs[T]                    // ndarray.reshape(*sh)
def reshape(sh: Array[Int]): Mohs[T]              // ndarray.reshape(sh)
/*AXIS*/ def repeat(reps: Int): Mohs[T]                    // ndarray.repeat(reps)
/*AXIS*/ def repeat(reps: Int*): Mohs[T]                   // ndarray.repeat(reps)
```

## Change axis

```scala
def transpose: Mohs[T]                   // np.transpose()
def reorderAxis(order: Int*): Mohs[T]    // np.swapaxis()
```

## Join arrays

TODO
```scala
def concat[T](axis: Int)(arrays: Mohs[T])     // np.concat(arrays, axis)
def stack[T](axis: Int)(arrays: Mohs[T])      // np.stack(arrays, axis)
```

## Split arrays

TODO

## Rearranging

```scala
def padStart(n: Int, fill: T): Mohs[T]      // pad(wid)
def padEnd(n: Int, fill: T): Mohs[T]        // pad(wid)
def reverse: Mohs[T]                        // reverse(axis)
def roll(n: Int): Mohs[T]                   // roll(n, axis)
```

## Produce index arrays

```scala
def nonZero: List[Mohs[Int]]                                 // ndarray.nonzero(): Mohs[Mohs[Int]]
def where[T](array: Mohs[Boolean], x: Mohs[T], y: Mohs[T])   // np.where(arr, x, y)
def indexes: Mohs[Mohs[Int]]                                 // np.indices(arr)
```

## Indexing

TODO
```scala
// np.choose(a, choices)
```

## Set

TODO
```scala
// np.intersect
// np.union
```