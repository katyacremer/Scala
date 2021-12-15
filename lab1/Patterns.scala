/** Напишите решение в виде функции.
 *
 * Синтаксис:
 *   val a: Int = ???
 *
 *   a match {
 *     case 0 => true
 *     case _ => false
 *   }
 */
object Patterns {

  sealed trait Hand
  case object Rock    extends Hand
  case object Paper   extends Hand
  case object Scissor extends Hand

  sealed trait Result
  case object Win  extends Result
  case object Lose extends Result
  case object Draw extends Result

  sealed trait Food
  case object Meat       extends Food
  case object Vegetables extends Food
  case object Plants     extends Food

  sealed trait Animal {
    val name: String
    val food: Food
  }
  case class Mammal(name: String, food: Food, weight: Int) extends Animal
  case class Fish(name: String, food: Food)                extends Animal
  case class Bird(name: String, food: Food)                extends Animal



  /* a) Напишите функцию, которая ставит в соответствие числу строку слудеющим образом:
   * Если:
   *     1 => "it is one"
   *     2 => "it is two"
   *     3 => "it is three"
   *     иначе => "what's that"
   */

  def f1(value: Int): String = value match{
    case 1 => "it is one"
    case 2 => "it is two"
    case 3 => "it is three"
    case _ => "what's that"
  }

  // примените вашу функцию из пункта (a) здесь, не изменяя сигнатуру
  def testIntToString(value: Int): String = f1(value)

  /* b) Напишите функцию которая возвращает true если переменная `value` принимает значение:
   *     "max" или "Max
   *     "moritz" или "Moritz"
   */

  def f2(value:String): Boolean = if(value == "max" || value == "Max" || value == "moritz" || value == "Moritz") true else false

  // примените функции из пункта (b) здесь, не изменяя сигнатуру
  def testIsMaxAndMoritz(value: String): Boolean = f2(value)

  // c) Напишите функцию проверки является ли `value` четным 

  def f3(value: Int): Boolean = value % 2 == 0

  // примените функции из пункта (c) здесь, не изменяя сигнатуру
  def testIsEven(value: Int): Boolean = f3(value)



  /* d) Напишите функцию, моделирующую игру в Камень ножницы бумага 
   *     1. камень побеждает ножницы
   *     2. ножницы побеждают бумагу
   *     3. бумага побеждает камень
   *    Выиграет ли игрок `a`?
   */

  def f4(a:Hand, b:Hand): Result = (a, b) match{
    case (Rock, Rock) | (Scissor, Scissor) | (Paper, Paper) => Draw
    case (Rock, Scissor) | (Scissor, Paper) | (Paper, Rock) => Win
    case (Rock, Paper) | (Paper, Scissor) | (Scissor, Rock) => Lose
  }

  // примените вашу функцию из пункта (d) здесь, не изменяя сигнатуру
  def testWinsA(a: Hand, b: Hand): Result = f4(a, b)



  // Примечание: используйте определение Animals

  // e) Верните вес (weight: Int) объекта Mammal, иначе верните -1.

  def f5(animal: Animal): Int = animal match{
    case m: Mammal => m.weight
    case _ => -1
  }

  // примените функцию из пункта (e) здесь, не изменяйте сигнатуру
  def testExtractMammalWeight(animal: Animal): Int = f5(animal)


  // f) Измените поле еда объектов классов Fishes и Birds на Plants, класс Mammels оставьте неизмененным.

  def f6(animal: Animal): Animal =  {
    animal match {
      case Fish(_,_) => new Fish(animal.name, Plants)
      case Bird(_,_) => new Bird(animal.name, Plants)
      case _ => animal
    }
  }

  // примените функцию из пункта (f) здесь, не изменяйте сигнатуру
  def testUpdateFood(animal: Animal): Animal = f6(animal)

  def main(args: Array[String]): Unit ={
    val monkey = Mammal("monkey", Vegetables, 32)
    val sparrow = Bird("sparrow", Vegetables)

    println("Напишите функцию, которая ставит в соответствие числу строку\n" + testIntToString(3));
    println("Напишите функцию которая возвращает true если переменная `value` принимает значение max, Max, moritz, Moritz\n" + testIsMaxAndMoritz("max"));
    println("Напишите функцию проверки является ли `value` четным\n" + testIsEven(8));
    println("Напишите функцию, моделирующую игру в Камень ножницы бумага\n" + testWinsA(Rock, Paper));

    println("Верните вес (weight: Int) объекта Mammal, иначе верните -1.\n" + testExtractMammalWeight(monkey));

    println("Измените поле еда объектов классов Fishes и Birds на Plants, класс Mammels оставьте неизмененным.")
    println("Было " + sparrow.food)
    println("Стало " + testUpdateFood(sparrow).food)


  }
}