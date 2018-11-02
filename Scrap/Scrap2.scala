
//Phuong Pham
//CS 558
//Fall 2018
//Quicksort in Scala

object Qsort
{
  //Swap two array elements, a[i] <-> a[j]
  def swap(a: Array[Int], i: Int, j: Int): Unit = 
  {
    val temp = a(i)
    a(i) = a(j)
    a(j) = temp
  }

  //Create an array to contain a random permutation of 1..N
  def createArray(n: Int): Array[Int] = 
  {
    val a = new Array[Int](n)
    for (i <- 0 until a.length)
      a(i) = util.Random.nextInt
    a
  }

  //Bubble sort for the base cases
  def bubblesort(a: Array[Int], low: Int, high: Int): Unit = 
  {
    val i
    if (low >= high)
      return
    for (i <- low until high)
    {
      for (j <- i+1 until high)
        if (a(i) > a(j))
          swap(a, i, j)
    }
  }

  //Partition an array range a[low, high] into two sub-ranges
  def partition(a: Array[Int], low: Int, high: Int): Int =
  {
    var pivot 
    var middle
    var i

    pivor = a(high)
    middle = low

    for (i <- low until high)
    {
      if (a(i) < pivot)
      {
        swap(a, i, middle)
        middle++
      }
    }
    swap(a, high, middle)
    middle
  }

  //Quicksort an array range a[low, high]
  def qsort(a: Array[Int], low: Int, high: Int): Unit = 
  {
    if (high - low < TODO)
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
  def printArray(a: Array[Int], N: Int): Unit = 
  {
    for (i <- 0 until N)
      println(a(i))
    println("\n")
  }

  //Verify a sorted array
  def verifyArray(a: Array[Int], N: Int): Unit = 
  {
    for (i <- 0 until N - 2)
    {
      if (a(i) > a(i + 1))
      {
        println("FAILED: a(i) = ")
	return
      }
    }
    println("Result verified")
  }

  //Main routine for testing quicksort
  def main(args: Array[String])
  {
    var a = Array[N]

    a = createArray(N)
    println("Initial array:\n")
    printArray(a, N)

    qsort(a, 0, N - 1)
    printf("After quicksort:\n")
    printArray(a, N)
    verifyArray(a, N)
  }
}
