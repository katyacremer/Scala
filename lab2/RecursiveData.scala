object RecursiveData {


  sealed trait List[A]
  case class Cons[A](head: A, tail: List[A]) extends List[A]
  case class Nil[A]() extends List[A]

  /** Напишите свои решения в виде функций. */
  object RecursiveData {

    // a) Реализуйте функцию, определяющую является ли пустым `List[Int]`.

    // используйте функцию из пункта (a) здесь, не изменяйте сигнатуру
    def testListIntEmpty(list: List[Int]): Boolean = list match{
      case _: Nil[Int] => true
      case _ => false
    }

    // b) Реализуйте функцию, которая получает head `List[Int]`или возвращает -1 в случае если он пустой.

    // используйте функцию из пункта (a) здесь, не изменяйте сигнатуру
    def testListIntHead(list: List[Int]): Int = list match{
      case list: Cons[Int] => list.head
      case _ => -1
    }

    // c) Можно ли изменить `List[A]` так чтобы гарантировать что он не является пустым,
    //если переопределить класс Nill и поставить в него значение

    /* d) Реализуйте универсальное дерево (Tree) которое хранит значения в виде листьев и состоит из:
     *      node - левое и правое дерево (Tree)
     *      leaf - переменная типа A
     */
    sealed trait Tree[A]
    case class Node[A] (left: Tree[A], right: Tree[A]) extends Tree[A]
    case class Leaf[A] (leaf: A) extends Tree[A]
  }
  def main(args: Array[String]): Unit ={
    println(RecursiveData.testListIntEmpty(Cons(1, Cons(2, Cons(3, Nil())))))
    println(RecursiveData.testListIntEmpty(Nil()))
    println(RecursiveData.testListIntHead(Cons(100, Cons(200, Cons(300, Nil())))))
    println(RecursiveData.testListIntHead(Nil()))
  }
}
