object SecondExercise {
  class MySeq(val s: Seq[Int]) {
    def sum: Int = {
      s.foldRight(0){_ + _}
    }

    def elem(i: Int): Option[Int] = {
      s.foldRight[(Option[Int], Int)]((None, s.length - 1)){case (elem, (result, position)) =>
        if (i == position) (Some(elem), position - 1)
        else (result, position - 1)
      }._1
    }

    def filter(items: Seq[Int]): Seq[Int] = {
      val distinctItems = items.distinct
      s.foldRight(Seq[Int]()){(element, sequence) =>
        if(distinctItems.contains(element)) sequence
        else element +: sequence
      }
    }

    def map[A](fn: Int => A): Seq[A] = {
      s.foldRight(Seq[A]()){(elem, sequence) =>
        fn(elem) +: sequence
      }
    }
  }

  object MySeq {
    def apply(s: Int*): MySeq = new MySeq(s.toSeq)
  }

  def main(args: Array[String]): Unit = {
    println("Test sum 1: " + (MySeq().sum == 0))
    println("Test sum 2: " + (MySeq(5).sum == 5))
    println("Test sum 3: " + (MySeq(1,2,3,4).sum == 10))

    println("Test elem 1: " + (MySeq().elem(0) == None))
    println("Test elem 2: " + (MySeq(2).elem(5) == None))
    println("Test elem 3: " + (MySeq(1,2,3,4).elem(0) == Some(1)))
    println("Test elem 4: " + (MySeq(1,2,3,4).elem(2) == Some(3)))
    println("Test elem 5: " + (MySeq(1,2,3,4).elem(3) == Some(4)))

    println("Test filter 1: " + (MySeq().filter(Seq(1,2,3)) == Seq()))
    println("Test filter 2: " + (MySeq(1,2,3).filter(Seq()) == Seq(1,2,3)))
    println("Test filter 3: " + (MySeq(1,2,3).filter(Seq(1,2)) == Seq(3)))
    println("Test filter 4: " + (MySeq(1,2,3).filter(Seq(2,1,2)) == Seq(3)))

    val fn: Int => String = x => x.toString
    println("Test map 1: " + (MySeq().map(fn) == Seq()))
    println("Test map 2: " + (MySeq(1,2,3).map(fn) == Seq("1", "2", "3")))
  }
}
