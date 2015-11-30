import scala.annotation.tailrec

package object lp {
  type Conjunto[T] = T => Boolean

  def contem[T](conj: Conjunto[T], elem: T) = conj(elem)
  def unitario[T](elem: T): Conjunto[T] = (e: T) => elem == e
  def uniao[T](c1: Conjunto[T], c2: Conjunto[T]): Conjunto[T] = (elem: T) => contem(c1, elem) || contem(c2, elem)
  def intersecao[T](c1: Conjunto[T], c2: Conjunto[T]): Conjunto[T] = (elem: T) => contem(c1, elem) && contem(c2, elem)
  def diferenca[T](c1: Conjunto[T], c2: Conjunto[T]): Conjunto[T] = (elem: T) => contem(c1, elem) && !contem(c2, elem)
  def filtro[T](c: Conjunto[T], f: T => Boolean): Conjunto[T] = (elem: T) => contem(c, elem) && f(elem)
  def map[T, U](c: Conjunto[T], f: U => T): Conjunto[U] = ???	
  
  trait ConjInt {
    def contem(x: Int): Boolean = ???
    def insere(x: Int): ConjInt = ???
    def uniao(outro: ConjInt): ConjInt = ???

    def filter(p: Int => Boolean): ConjInt = ???
    def map(f: Int => Int): ConjInt = ???
    def flatMap(f: Int => ConjInt): ConjInt = ???
  }
  case class ConjVazio() extends ConjInt
  case class ConjCons(elem: Int, esq: ConjInt, dir: ConjInt) extends ConjInt
    
  def intersecao(c1: ConjInt, c2: ConjInt): ConjInt = ???
}
