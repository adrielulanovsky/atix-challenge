object ThirdExercise {
  sealed trait Frame {
    def roll(pins: Int): Option[Frame]
    def score(nextShot: Int, followingShot: Int): Int
    val shotsList: List[Int]
    lazy val sum = shotsList.sum
  }

  case class NormalFrame(shots: (Int, Option[Int])) extends Frame {
    override val shotsList: List[Int] = shots match {
      case (r, None) => List(r)
      case (r, Some(s)) => List(r,s)
    }
    assert(sum <= 10)
    lazy val isStrike = shots._1 == 10
    lazy val isSpare = sum == 10 && !isStrike
    lazy val isOpen = sum < 10

    override def roll(pins: Int): Option[Frame] = {
      val canRoll = shots._2.isEmpty && shots._1 < 10
      if (canRoll) Some(NormalFrame(shots.copy(_2 = Some(pins))))
      else None
    }

    override def score(nextShot: Int, followingShot: Int): Int = {
      if (isStrike) 10 + nextShot + followingShot
      else if (isSpare) 10 + nextShot
      else sum
    }
  }

  case class TenthFrame(shots: (Int, Option[Int], Option[Int])) extends Frame {
    override lazy val shotsList: List[Int] = shots match {
      case (r, None, None) => List(r)
      case (r, Some(s), None) => List(r,s)
      case (r, Some(s), Some(t)) => List(r,s,t)
      case _ => throw new Exception("Missing middle roll in tenth frame")
    }

    override def roll(pins: Int): Option[Frame] = shots match {
      case (r, None, None) => Some(TenthFrame((r, Some(pins), None)))
      case (r, Some(s), None) if r == 10 || r + s == 10 => Some(TenthFrame((r, Some(s), Some(pins))))
      case _ => None
    }

    override def score(nextShot: Int, followingShot: Int): Int = sum
  }

  case class State(reversedFrames: List[Frame]) {
    def roll(pins: Int): State = {
      val newFrames: List[Frame] = {
        if (reversedFrames.isEmpty)
          List(NormalFrame((pins, None)))
        else {
          val maybeLastFrame = reversedFrames.headOption.flatMap(_.roll(pins))
          maybeLastFrame.fold{
            if (reversedFrames.length == 9) TenthFrame(pins, None, None) :: reversedFrames
            else NormalFrame(pins, None) :: reversedFrames
          }
          { lastFrame => lastFrame :: reversedFrames.tail }
        }
      }
      State(newFrames)
    }

    def score(): Int = {
      val scoreHelper: (Int, (Int, Int)) = reversedFrames.foldLeft {
        (0, (0, 0))
      } { case ((score, nextTwoShots), frame) =>
        val newNextTwoShots: (Int, Int) = frame match {
          case TenthFrame((r, Some(s), _)) => (r,s)
          case TenthFrame((r,_, _)) => (r, 0)
          case NormalFrame((r, Some(s))) => (r,s)
          case NormalFrame((r, _)) => (r, nextTwoShots._1)
        }
        (score + (frame.score _).tupled(nextTwoShots), newNextTwoShots)
      }
      scoreHelper._1
    }
  }

  def main(args: Array[String]): Unit = {
    def rollList(state: State, pins: List[Int]): State = {
      pins.foldLeft{state}{(acc, pin) =>acc.roll(pin) }
    }
    val initialGameState = State(List())
    println("Test 1: " + (rollList(initialGameState, List.fill(20)(1)).score() == 20))
    println("Test 2: " + (rollList(initialGameState, List.fill(12)(10)).score() == 300))
    println("Test 3: " + (rollList(initialGameState, List.fill(21)(5)).score() == 150))
  }
}
