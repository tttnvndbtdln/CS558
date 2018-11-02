import scala.io.StdIn.{readLine, readInt}
import scala.util.Random

object Constants
{
  val minsize = 100
}

object Qsort
{
  def swap(a: Array[Int], i: Int, j: Int): Unit =
  {
    val temp = a(i)
    a(i) = a(j)
    a(j) = temp
  }

  def createArray(n: Int): Array[Int] =
  {
    //List.fill[Int](n)(1)
    //var a = 0 to scala.util.Random.nextInt(10)
    //val a = new Array[Int](n)
    Stream.continually(Random.nextInt(100) + 1).take(n).toArray //Between 10 and 100
    //Stream.continually(Random.nextInt(100)).take(n).toArray //Between 0 and 100
    //Stream.continually(Random.nextInt).take(n).toArray //Any int
  }

  def bubblesort(a: Array[Int], low: Int, high: Int): Unit =
  {
    if (low >= high)
      return
    for (i <- low until high)
    {
      for (j <- i + 1 until high)
      {
        if (a(i) > a(j))
	  swap(a, i, j)
      }
    }
  }

  def partition(a: Array[Int], low: Int, high: Int): Int = 
  {
    var pivot = a(high)
    var middle = low

    for (i <- low until high)
    {
      if (a(i) < pivot)
      {
        swap(a, i, middle)
	middle += 1
      }
    }

    swap(a, high, middle)
    return middle
  }

  def qsort(a: Array[Int], low: Int, high: Int): Unit = 
  {
    if (high - low < Constants.minsize)
      bubblesort(a, low, high)
    else
    {
      var middle = partition(a, low, high)
      if (low < middle)
        qsort(a, low, middle - 1)
      if (middle < high)
        qsort(a, middle + 1, high)
    }
  }

  def printArray(a:Array[Int]): Unit = 
  {
    println(a.mkString(" "))
    //for (i <- 0 until a.length)
      //println(a(i))
  }

  def verifyArray(a: Array[Int], N: Int): Unit = 
  {
    for (i <- 0 until N - 2)
    {
      if (a(i) > a(i + 1))
      {
        println(s"\nFAILED: a($i) = " + a(i) + s" a(${i+1}) = " + a(i+1))
	return
      }
    }
    println("Result verified!\n")
  }

  def main(args: Array[String])
  {
    
    println("\nHello.")
    println("\nEnter number of array elements:")
    val n = scala.io.StdIn.readInt()
    val a = createArray(n)
    printArray(a)
    
    qsort(a, 0, n)

    println("Array afterward:")
    printArray(a)

    println("Exit")
  }
}

//val res = formatArgs(Array("zero", "one", "two"))
//assert(rex == "zero\none\ntwo")
/*println("Enter first ith element to switch:")
    val i = scala.io.StdIn.readInt()
    println("Enter second ith element to switch:")
    val j = scala.io.StdIn.readInt()*/

