package utils

import scala.reflect.ClassTag

object Utils {
  def map2dArray[A: ClassTag](l: Array[Array[A]], f: (Int, Int) => A): Array[Array[A]] = {
    l.zipWithIndex.map {
      case (xel, x) =>
        xel.zipWithIndex.map {
          case (_, y) => {
            f(x, y)
          }
        }
    }
  }
}
