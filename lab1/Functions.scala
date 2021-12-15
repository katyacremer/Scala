import scala.math

/** Напишите отдельные функции, решающие поставленную задачу.
 *
 * Синтаксис:
 * // метод
 * def myFunction(param0: Int, param1: String): Double = // тело
 *
 * // значение
 * val myFunction: (Int, String) => Double (param0, param1) => // тело
 */
object Functions {

  /* a) Напишите функцию, которая рассчитывает площадь окружности
   *    r^2 * Math.PI
   */
  def area(radius: Double): Double = radius * radius * Math.PI


  // примените вашу функцию из пункта (a) здесь, не изменяя сигнатуру
  def testCircle(r: Double): Double = area(r)


  /* b) Напишите карированную функцию которая рассчитывает площадь прямоугольника a * b.
   */
  def sqArea1(a: Double)(b: Double): Double = a * b


  // примените вашу функцию из пукта (b) здесь, не изменяя сигнатуру
  def testRectangleCurried(a: Double, b: Double): Double = sqArea1(a)(b)


  // c) Напишите не карированную функцию для расчета площади прямоугольника.

  def sqArea2(a: Double, b: Double): Double = a * b

  // примените вашу функцию из пункта (c) здесь, не изменяя сигнатуру
  def testRectangleUc(a: Double, b: Double): Double = sqArea2(a, b)

  def main(args: Array[String]): Unit = {
    println("Площадь окружности: " + testCircle(5))
    println("Карированная функция высчитывает площадь прямоугольника: " + testRectangleCurried(2, 3))
    println("Некарированная функция высчитывает площадь прямоугольника: " + testRectangleUc(2, 3))
  }
}
