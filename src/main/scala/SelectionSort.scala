object SelectionSort {
  def selectionSort[A](as: List[A])(sorted: (A, A) => Boolean): List[A] = {
    def foldLeft[B](as: List[A], b: B)(f: (B, A) => B): B = as match {
      case Nil => b
      case head :: tail => foldLeft(tail, f(b, head))(f)
    }
    def ff(as: List[A]): List[A] = {
      foldLeft(as, List.empty[A]) {
        case (Nil, a) => List(a)
        case (head :: tail, a) => if (sorted(head, a)) head :: a :: tail else a :: head :: tail
      }
    }
    def f(as: List[A]): List[A] = {
      ff(as) match {
        case Nil => Nil
        case head :: tail => head :: f(tail)
      }
    }
    f(as)
  }
  def selectionSort[A](as: Array[A])(sorted: (A, A) => Boolean): Array[A] = ???
}