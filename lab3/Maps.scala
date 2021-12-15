import scala.annotation.tailrec

/** Напишите вашу реализацию в тестовые функции.
 *
 * https://docs.scala-lang.org/overviews/collections/maps.html
 */
object Maps {

  case class User(name: String, age: Int)

  /* a) В данной Seq[User] сгруппируйте пользователей по имени (`groupBy`) и вычислите средний возраст: `name -> averageAge`
   *    Вы можете реализовать ваше решение в теле тестовой функции. Не изменяйте сигнатуру.
   */
  def testGroupUsers(users: Seq[User]): Map[String, Int] = {
    def func(seq: Seq[Int]) = seq.sum / seq.length
    users.groupBy(_.name).map{
      inpt => (inpt._1, func(inpt._2.map(_.age)))
    }
  }

  /* b) Дана `Map[String, User]` состоящая из имен пользователей `User`, сколько имен пользователей, содержащихся в Map, содержат подстроку "Adam"?
   *    Вы можете реализовать ваше решение в теле тестовой функции. Не изменяйте сигнатуру.
   */
  def testNumberFrodos(map: Map[String, User]): Int = {
      map.count(x=>x._2.name=="Adam")
  }

  /* c) Удалите всех пользователей возраст которых менее 35 лет.
   *    Вы можете реализовать ваше решение в теле тестовой функции. Не изменяйте сигнатуру.
   */
  def testUnderaged(map: Map[String, User]): Map[String, User] = {
    map.filter(v => v._2.age > 35)
  }

  def main(args: Array[String]): Unit = {
    val user1 = User("Bobby", 12)
    val user2 = User("Bobby", 18)
    val user3 = User("Alice", 16)
    var users: Seq[User] = Seq(user1, user2, user3)
    println(testGroupUsers(users))

    var map = Map("One" -> User("Adam", 12), "Two" -> User("Adam", 15), "Three" -> User("Alice", 46))
    println(testNumberFrodos(map))
    println(testUnderaged(map))
  }
}