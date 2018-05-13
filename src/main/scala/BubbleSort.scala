object BubbleSort {
  def bubbleSort[A](as: List[A])(sorted: (A, A) => Boolean): List[A] = {
    def f(source: List[A], result: List[A]): List[A] = source match {
      case Nil => result
      case _ => ff(source, Nil, result)
    }
    def ff(source: List[A], temp: List[A], result: List[A]): List[A] = source match {
      case head :: Nil => f(temp, head :: result)
      case head1 :: head2 :: tail => if (sorted(head1, head2)) ff(head2 :: tail, head1 :: temp, result) else ff(head1 :: tail, head2 :: temp, result)
    }
    f(as, Nil)
  }

  def bubbleSort[A](as: Array[A])(sort: (A, A) => Boolean): Array[A] = {
    for {
      i <- 0 until as.length - 1
      j <- 0 until as.length - 1 - i
    } {
      if (sort(as(j + 1), as(j))) {
        val temp = as(j)
        as(j) = as(j + 1)
        as(j + 1) = temp
      }
    }
    as
  }
}