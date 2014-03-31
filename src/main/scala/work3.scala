package nthu.work3


object hw3 {
  type ??? = Nothing

  val tolerance = 0.001

  // compute the absolute value of x
  def abs(x: Double): Double =
    if (x>=0) x
    else -x

  // compute x square
  def square(x: Double): Double = x*x

  // compute the average of x and y
  def average(x: Double, y: Double): Double = (x+y)/2

 
  // sqrt: compute the square root of x
  // algorithm: start with an initial guess, 
  def sqrt(x: Double): Double = {
    /*
     * goodEnough: check if the square of guess is close enough to x
     * By 'close enough' we mean the absolute difference between
     * the square of guess and x is smaller than tolerance (0.001)
     */
    def goodEnough(guess: Double, x: Double) = abs(square(guess) - x) < tolerance 


    // set the improved guess as the average of guess and x/guess
    def improve(guess: Double, x: Double): Double = average (guess, x/guess)

    /*
     * If the current guess is good enough, the approximate answer is guess.
     * Otherwise, recursively call sqrtIter with the improved guess.
     */
    def sqrtIter(guess: Double, x: Double): Double =
      if (goodEnough (guess, x))
        guess
      else
        sqrtIter(improve (guess, x), x)

    sqrtIter(1.0, x)}

  /*
   * If neg and pos are close enough (absolute difference is smaller than tolerance)
   * the approximate solution is the average of neg and pos
   * Otherwise, let the average of neg and pos be mid, and evaluate function f at mid.
   * If the function value of mid is positive, recursively call search to find a solution
   * within the range of [neg, mid].
   * If the function value of mid is negative, recursively call search to find a solution
   * within the range of [mid, pos].
   * Otherwise, mid is the approximation solution.
   */
  def search(f: Double => Double, neg: Double, pos: Double): Double  =
    {
      val mid = average (neg,pos)
      if (abs (neg - pos) < tolerance) mid
      else
      { 
        if (f(mid) >= 0) search (f, neg, mid)
        else search (f, mid, pos)
      }
    }

  /*
   * Find the fix point of function f.
   * That is, find x such that x = f(x).
   * Evaluate f at some point x, if x and f(x) is close enough,
   * then x is the fixed point; otherwise let f(x) as the next x and recursively evaluate f.
   */
  def fixedPoint(f: Double => Double, firstGuess: Double): Double = 
    {
      if (abs(f (firstGuess) - firstGuess) < tolerance) firstGuess
      else fixedPoint (f, f (firstGuess))
    }

  // If your implementation of fixedPoint is correct,
  // you can use sqrtFix to compute the square root of x.
  def sqrtFix(x: Double): Double = 
    fixedPoint( y => average(y, x/y), 1.0)

  // Let dx be 0.00001
  // The derivative of g is defined by  ( g(x+dx) - g(x) ) / dx
  def derivative(g: Double => Double): Double => Double =
    {
      val dx = 0.00001
      (x:Double) => (g (x+dx) - g (x) ) / dx
    }

  // newtonTransform defines the recurrence process of Newton's method
  // x_new = x_old  - g(x_old)/ derivative of g(x_old)
  def newtonTransform(g: Double => Double): Double => Double =
    {
      (x:Double) => x- g (x) / derivative (g) (x)
    }

  def newtonMethod(g: Double => Double, guess: Double): Double = {
    fixedPoint(newtonTransform(g), guess)
  }

  // If your implementations of fixedPoint, derivative, and newtonTransform are correct,
  // you can use sqrtNewton to compute the square root of x.
  def sqrtNewton(x: Double): Double = {
    newtonMethod( (y:Double) => square(y)-x, 1.0)
  }

  def main(arg: Array[String]) {
    println(abs(-3.0))

    val f = (x: Double) => square(x)-3.0
    println(search(f, 1.0, 3.0))

    println(sqrt(3.0))
    println(sqrtFix(3.0))
    println(sqrtNewton(3.0))
    println(sqrtNewton(4.0))
  }
}

