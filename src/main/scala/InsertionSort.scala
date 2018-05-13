object InsertionSort {
  def insertionSort[A](as: List[A])(sorted: (A, A) => Boolean): List[A] = {
    def insert(as: List[A], a: A): List[A] = as match {
      case Nil => List(a)
      case head :: tail => if (sorted(a, head)) a :: as else head :: insert(tail, a)
    }
    def foldLeft[A,B](as: List[A], b: B)(f: (B, A) => B): B = as match {
      case Nil => b
      case head :: tail => foldLeft(tail, f(b, head))(f)
    }
    foldLeft(as, List.empty[A])(insert)
  }

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