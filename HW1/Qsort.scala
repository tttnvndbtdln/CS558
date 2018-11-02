//Phuong Pham
//CS 558
//Fall 2018
//Quicksort in Scala

import scala.io.StdIn.{readLine, readInt}
import scala.util.Random

object Constants
{
  val minsize = 10
}

object Qsort
{
  val usage = "\nUsage: Enter a positive integer to generate an array of random numbers.\n"

  //Swap two array elements, a(i) <-> a(j)
  def swap(a: Array[Int], i: Int, j: Int): Unit = 
  {
    if (i == j)
      return
    val temp = a(i)
    a(i) = a(j)
    a(j) = temp
  }

  //Create an array to contain a random permutation of 1..n
  def createArray(n: Int): Array[Int] = 
  {
    Stream.continually(Random.nextInt).take(n).toArray
  }

  //Bubble sort for the base cases
  def bubblesort(a: Array[Int], low: Int, high: Int): Unit = 
  {
    if (low >= high)
      return
    for (i <- low to high)
    {
      for (j <- i + 1 to high)
      {
        if (a(i) > a(j))
          swap(a, i, j)
      }
    }
  }

  //Partition an array range a[low, high] into two sub-ranges
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

  //Quicksort an array range a(low, high)
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

  //Print an array
  def printArray(a: Array[Int]): Unit = 
  {
    println(a.mkString(", "))
  }

  //Verify a sorted array
  def verifyArray(a: Array[Int], n: Int): Unit = 
  {
    for (i <- 0 until n - 1)
    {
      if (a(i) > a(i + 1))
      {
        println(s"\nFAILED: a($i) = " + a(i) + s" a(${i+1}) = " + a(i+1))
	return
      }
    }
    println("\nResult verified!")
  }

  //Main routine for testing quicksort
  def main(args: Array[String])
  {
    if (args.length != 1)
    {
      println(usage)
      return
    }

    val n = args(0).toInt
    if (n < 1)
    {
      println(usage)
      return
    }
    if (n == 1)
      println("\nSorting a 1 item list? Alright, here it is:")

    println(s"\nGenerating an array of $n random elements...")
    val a = createArray(n)
    println("\nInitial array:")
    printArray(a)

    println("\nSorting array...")
    qsort(a, 0, n - 1)
    
    printf("\nAfter quicksort:\n")
    printArray(a)

    if (n == 1)
      println("\nThat was fun.")

    verifyArray(a, n)
  }
}
