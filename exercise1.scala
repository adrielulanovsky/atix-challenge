import scala.annotation.tailrec
import scala.collection.immutable.Seq
import java.security.MessageDigest

object FirstExercise {
  case class Node(someData: Seq[Byte])

  def hash(data: Seq[Byte]): Seq[Byte] =
    MessageDigest
      .getInstance("MD5")
      .digest(data.toArray[Byte]).to[Seq]

  def merkleRootHash(node: Node*): Seq[Byte] = {
    val childrenHashes: Seq[Byte] = collection.immutable.Seq(node: _*).flatMap{n => hash(n.someData)}
    hash(childrenHashes)
  }

  def main(args: Array[String]): Unit = {
    val merkleRoot = merkleRootHash(
      Node(Seq(1, 1)),
      Node(Seq(0, 8)),
      Node(Seq(7, 10)),
      Node(Seq(2, 3)),
      Node(Seq(1))
    )

    println(merkleRoot)

    val childrenHashes = hash(
      hash(Seq(1, 1)) ++
      hash(Seq(0, 8)) ++
      hash(Seq(7, 10)) ++
      hash(Seq(2, 3)) ++
      hash(Seq(1))
    )

    println("Test: " + (merkleRoot == childrenHashes))
  }
}

