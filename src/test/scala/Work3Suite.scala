package nthu.work3

import org.scalatest._

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner


/**
  * Two options:
  *  1. run the "test" command in the SBT console
  *  2. right-click the file in eclipse and choose "Run As" -> "JUnit Test"
  */
  
@RunWith(classOf[JUnitRunner])
class Work3Suite extends FunSuite {

  import nthu.work3.hw3._

  def closeEnough(x: Double, y: Double) = {
    val tolerance = 0.001
    val abs = (v: Double) =>  if (v>0) v else -v
    abs(x-y) < tolerance
  }
  test("test abs OK") {
    assert(abs(-3.0)==3.0);
    assert(abs(5.0)==5.0);
  }

  test("test square OK") {
    assert(square(3.0)==9.0)
  }

  test("test average OK") {
    assert(average(3.0,3.0)==3.0)
    assert(average(-3.0,3.0)==0.0)
  }

  test("test sqrt OK") {
    assert(closeEnough(sqrt(4.0), 2.0))
  }

  test("test search OK") {
    val f = (x: Double) => square(x)-3.0
    assert(closeEnough(search(f, 1.0, 3.0), 1.732))
  }

  test("test fixedPoint") {
    def cbrt(x: Double) = (y: Double) => average(y, x/square(y))
    assert(closeEnough(fixedPoint(cbrt(-8.0), 1.0),-2.0))
  }

  test("test sqrtFix OK") {
    assert(closeEnough(sqrtFix(4.0), 2.0))
  }

  test("test derivative OK") {
    def cube(x: Double) = x*x*x
    assert(closeEnough(derivative(cube)(4.0), 48.0))
  }

  test("test sqrtNewton OK") {
    assert(closeEnough(sqrtNewton(4.0), 2.0))
  }

}
