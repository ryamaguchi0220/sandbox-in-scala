object InsertionSort {
  // TODO immutable insertionSort
  def insertionSort[A](as: List[A])(sorted: (A, A) => Boolean): List[A] = ???
  def insertionSort[A](as: Array[A])(sorted: (A, A) => Boolean): Array[A] = {
    (1 until as.length).foreach { i =>
      var j = i
      while(j > 0 && !sorted(as(j - 1), as(j))) {
        val temp = as(j)
        as(j) = as(j - 1)
        as(j - 1) = temp
        j = j - 1
      }
    }
    as
  }
}