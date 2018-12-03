import java.io.{BufferedWriter, File, FileWriter}

import scala.collection.immutable

object Main extends App {
    sealed case class Norm(matrix: Array[Array[Double]], k: Int, n: Int) extends Runnable {
        override def run(): Unit = {
            matrix(k)(n) /= matrix(k)(k)
            matrix(k)(k) = 1
        }
    }
    sealed case class Factor(matrix: Array[Array[Double]], i: Int, k: Int, factors: Array[Double]) extends Runnable {
        override def run(): Unit = {
            factors(k) = matrix(k)(i) / matrix(i)(i)
        }
    }
    sealed case class Subtract(matrix: Array[Array[Double]], i: Int, k: Int, factors: Array[Double]) extends Runnable {
        override def run(): Unit = {
            matrix(k) = matrix(k).zip(matrix(i).map(_ * factors(k))).map(a ⇒ a._1 - a._2)
        }
    }
    
    def swap_rows(index: Int, array: ⇒ Array[Array[Double]]): Unit = {
        val index2: Int = array.zipWithIndex.dropWhile(a ⇒ a._1(index) == 0 || a._2 < index).head._2
        val tmp = array(index)
        array(index) = array(index2)
        array(index2) = tmp
    }
    
    val matrix: Array[Array[Double]] = io.Source.fromFile("resource/in.txt")
        .getLines()
        .map(_ split " " map (_.trim toDouble)).filter(_.length > 1)
        .toArray.transpose
    
    val n = matrix.length
    
    for (i ← 0 until n) {
        val factors: Array[Double] = new Array(n)
        
        if (matrix(i)(i) == 0.0)
            swap_rows(i, matrix)
        
        val factorThreads = (0 until n toList).foldLeft(List.empty[Thread])((threads, k) ⇒ {
            threads ++ List(new Thread(Factor(matrix, i, k, factors)))
        })
        
        factorThreads.foreach(_.start())
        factorThreads.foreach(_.join())
        
        
        val subThreads: immutable.Seq[Thread] = (0 until n toList).foldLeft(List.empty[Thread])((threads, k) ⇒ {
            if (k != i) {
                threads ++ List(new Thread(Subtract(matrix, i, k, factors)))
            } else {
                threads
            }
        })
        
        subThreads.foreach(_.start())
        subThreads.foreach(_.join())
    }
    
    val normThreads: immutable.Seq[Thread] = (0 until n toList).foldLeft(List.empty[Thread])((threads, k) ⇒ {
        threads ++ List(new Thread(Norm(matrix, k, n)))
    })
    
    normThreads.foreach(_.start())
    normThreads.foreach(_.join())
    
    val file = new File("output.txt")
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(f"$n\n")
    matrix.transpose.foreach(x ⇒ bw.write(f"${x.deep.mkString(" ")}\n"))
    bw.close()
}