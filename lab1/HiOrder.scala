object HiOrder {
  val plus: (Int, Int) => Int = _ + _
  val multiply: (Int, Int) => Int = _ * _

  /* a) Напишите функцию, которая принимает `f: (Int, Int) => Int`, параменты `a` и `b`
   *    и коэффициент умножения `n` и возвращает n * f(a, b). Назовите `nTimes`.
   */

  def nTimes(f: (Int, Int) => Int, a: Int, b: Int, n: Int): Int = n * f(a, b)

  // примените вашу функцию (a) здесь, не изменяйте сигнатуру
  def testNTimes(f: (Int, Int) => Int, a: Int, b: Int, n: Int): Int = nTimes(plus, a, b, n)

  /* b) Напишите анонимную функцию, функцию без идентификатора ((a, b) => ???) для `nTimes` которая
   *    выполняет следующее:
   *          if (a > b) a else b
    */
  val anon = (_: Int) max (_: Int)

  def testAnonymousNTimes(a: Int, b: Int, n: Int): Int = n * anon(a, b)

  def main(args: Array[String]): Unit = {
    println("Функция nTimes выводит n * (a + b): \n" + nTimes(plus, 12, 5, 3))
    println("Функция с применением анонимной функции для нахождения максимума, результат которой умножается на n:\n" + testAnonymousNTimes(12, 5, 3))
  }
}
