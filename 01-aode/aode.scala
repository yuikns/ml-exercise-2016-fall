import java.io.{File, PrintWriter}

import scala.collection.mutable.{ListBuffer, Map => MMap}
import scala.io.Source

/**
  *
  * @author Yu Jing <yu@argcv.com> on 10/9/16
  */
@SerialVersionUID(1457976278L)
case class Vec(value: Array[Double]) extends Serializable {
  override def toString = s"[${value.mkString(", ")}]"

  def size: Int = length

  def length: Int = value.length

  def indices: Range = value.indices

  def apply(i: Int): Double = value(i)
}

/**
  *
  * @author Yu Jing <yu@argcv.com> on 10/9/16
  */
case class LabeledPoint(label: Double, features: Vec) extends Serializable {
  override def toString: String = {
    s"($label,$features)"
  }

  def length = features.length

  def y = label

  def x = features

}

/**
  *
  * @author Yu Jing <yu@argcv.com> on 10/9/16
  */
object AODE extends App {
  val ipath = if (args.length > 0) args(0) else "watermelon.txt"
  val opath = if (args.length > 1) args(1) else "result.txt"
  val lpath = if (args.length > 2) args(2) else "labels.txt"
  val (data: Array[LabeledPoint], labels: Array[Array[String]]) = loadPoints()

  // feature-offset, value, ids
  val gStatsMap: Map[Int, Map[Double, List[Int]]] = {
    val g = MMap[Int, MMap[Double, ListBuffer[Int]]]()
    data.zipWithIndex.foreach { case (p, j) =>
      p.features.indices.foreach { i =>
        val m = g.getOrElseUpdate(i, MMap[Double, ListBuffer[Int]]())
        val fval: Double = p.features(i)
        m.getOrElseUpdate(fval, ListBuffer[Int]()).append(j)
      }
    }
    g.map(kv => kv._1 -> kv._2.map(e => e._1 -> e._2.toList).toMap).toMap
  }

  // label, feature-offset, value, ids
  val cStatsMap: Map[Double, Map[Int, Map[Double, List[Int]]]] = {
    val c = MMap[Double, MMap[Int, MMap[Double, ListBuffer[Int]]]]()
    data.zipWithIndex.foreach { case (p, j) =>
      val cm = c.getOrElseUpdate(p.label, MMap[Int, MMap[Double, ListBuffer[Int]]]())
      p.features.indices.foreach { i =>
        val m = cm.getOrElseUpdate(i, MMap[Double, ListBuffer[Int]]())
        val fval: Double = p.features(i)
        m.getOrElseUpdate(fval, ListBuffer[Int]()).append(j)
      }
    }
    c.map(ckv => ckv._1 -> ckv._2.map(kv => kv._1 -> kv._2.map(e => e._1 -> e._2.toList).toMap).toMap).toMap
  }


  saveLabels()
  saveResults()

  def saveLabels(): Unit = {
    val pw = new PrintWriter(new File(lpath))
    labels.indices.foreach { i =>
      labels(i).indices.foreach { j =>
        pw.append(i.toString).
          append("\t").
          append(j.toString).
          append("\t").
          append(labels(i)(j)).
          append("\n")
      }
    }
    pw.flush()
    pw.close()
  }

  def loadPoints(): (Array[LabeledPoint], Array[Array[String]]) = {
    val lines = Source.fromFile(new File(ipath)).getLines().toArray.splitAt(1)
    val lbuff = lines._1.head.split(",").drop(2).indices.map(_ => MMap[String, Int]()).toArray
    def doubleToLabel(score: Double): String = {
      (score * 5).toInt.toString
      //      match {
      //        case 0 =>
      //          "[0,0.6)"
      ////          "[0,0.333)"
      ////        case 1 =>
      ////          "[0.333,0.667)"
      //        case _ =>
      ////          "[0.667,-)"
      //          "[0.5,-)"
      //      }
    }
    val lp: Array[LabeledPoint] = lines._2.map { l =>
      val elems = l.split(",").drop(1)
      val cols = elems.splitAt(elems.length - 1)
      val label = if (cols._2.head == "是") 1.0 else -1.0
      val features: List[Double] = cols._1.indices.map { i =>
        val s = if (i == 6 || i == 7) {
          doubleToLabel(cols._1(i).toDouble)
        } else {
          cols._1(i)
        }
        (lbuff(i).get(s) match {
          case Some(v) =>
            v
          case None =>
            val v = lbuff(i).size
            lbuff(i).put(s, v)
            v
        }).toDouble
      }.toList
      //println(s"labels: ${features.mkString("#")}")
      LabeledPoint(label, Vec(features.toArray))
    }
    (lp, lbuff.map { kv => kv.toArray.map(e => (e._2, e._1)).sortWith(_._1 < _._1).map(_._2) })
  }

  def saveResults(): Unit = {
    val pw = new PrintWriter(new File(opath))
    data.foreach { (p: LabeledPoint) =>
      //      println(s"p: ${p.toString}")
      //      pw.append(if (p.y > 0) "Y" else "N").append("\t")
      val res = List[Double](1.0, -1.0).map { c =>
        labels.indices.flatMap { i => // 0 .. x
          if (Dxi(p, i) >= 0) {
            Some((Dcxi(c, p, i).toDouble + 1) / (D() + N(i)) *
              labels.indices.foldLeft(1.0) { (l, j) =>
                val Pxjcxi = (Dcxixj(c, p, i, j).toDouble + 1) / (Dcxi(c, p, i) + N(j))
                l * Pxjcxi
              })
          } else {
            None
          }
        }.sum
      }
      val rate = 1.0 / res.sum
      //      if (res.head > res.last) {
      //        pw.append("Y\t")
      //      } else {
      //        pw.append("N\t")
      //      }
      pw.append(res.map(_ * rate).mkString(",")).write("\n")
    }
    pw.flush()
    pw.close()
  }

  def D() = data.length

  def N(i: Int) = labels(i).length

  def Dxi(p: LabeledPoint, i: Int): Int = C(i, p.x(i)).length

  def C(i: Int, score: Double): List[Int] = {
    gStatsMap.get(i) match {
      case Some(m) =>
        m.get(score) match {
          case Some(v) =>
            v
          case None =>
            List[Int]()
        }
      case None =>
        List[Int]()
    }
  }

  def Dcxi(c: Double, p: LabeledPoint, i: Int): Int = C(c, i, p.x(i)).length

  def C(c: Double, i: Int, score: Double): List[Int] = {
    cStatsMap.get(c) match {
      case Some(g) =>
        g.get(i) match {
          case Some(m) =>
            m.get(score) match {
              case Some(v) =>
                v
              case None =>
                List[Int]()
            }
          case None =>
            List[Int]()
        }
      case None =>
        List[Int]()
    }
  }

  def Dcxixj(c: Double, p: LabeledPoint, i: Int, j: Int) = {
    val si: Set[Int] = C(c, i, p.x(i)).toSet
    C(c, j, p.x(j)).count(si.contains)
  }

}

AODE.main(args)
