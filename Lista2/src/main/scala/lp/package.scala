import scala.annotation.tailrec

package object lp {
  type Conjunto[T] = T => Boolean

  def contem[T](conj: Conjunto[T], elem: T) = conj(elem)
  def unitario[T](elem: T): Conjunto[T] = (e: T) => elem == e
  def uniao[T](c1: Conjunto[T], c2: Conjunto[T]): Conjunto[T] = (elem: T) => contem(c1, elem) || contem(c2, elem)
  def intersecao[T](c1: Conjunto[T], c2: Conjunto[T]): Conjunto[T] = (elem: T) => contem(c1, elem) && contem(c2, elem)
  def diferenca[T](c1: Conjunto[T], c2: Conjunto[T]): Conjunto[T] = (elem: T) => contem(c1, elem) && !contem(c2, elem)
  def filtro[T](c: Conjunto[T], f: T => Boolean): Conjunto[T] = (elem: T) => contem(c, elem) && f(elem)
  def map[T, U](c: Conjunto[T], f: U => T): Conjunto[U] = (elem: U)	=> contem(c, f(elem))
  
  trait ConjInt {
    def contem(x: Int): Boolean = this match {
      case ConjVazio() => false
      case ConjCons(elem, esq, dir) => x == elem || esq.contem(x) || dir.contem(x) 
    }
    
    def insere(x: Int): ConjInt = this match {
      case ConjVazio() => ConjCons(x, ConjVazio(), ConjVazio())
      case ConjCons(elem, esq, dir) => (esq, dir) match { 
        case (ConjVazio(), ConjVazio()) => ConjCons(elem, ConjCons(x, ConjVazio(), ConjVazio()), ConjVazio())
        case (ConjVazio(), ConjCons(_, _, _)) => ConjCons(elem, ConjCons(x, ConjVazio(), ConjVazio()), dir)
        case (ConjCons(_, _, _), ConjVazio()) => ConjCons(elem, esq, ConjCons(x, ConjVazio(), ConjVazio()))
        case (ConjCons(_, _, _), ConjCons(_, _, _)) => ConjCons(elem, esq.insere(x), dir)
      }
    }
    def uniao(outro: ConjInt): ConjInt = outro match {
      case ConjVazio() => this
      case ConjCons(elem, ConjVazio(), ConjVazio()) => this.insere(elem)
      case ConjCons(elem, esq, dir) => this.uniao(esq).uniao(dir) 
    }

    def filter(p: Int => Boolean): ConjInt = {
      def loop(aux: ConjInt)= this match {
        case ConjVazio() => ConjVazio()
        case ConjCons(elem, esq, dir) => if(p(elem)) aux.insere(elem).uniao(esq.filter(p)).uniao(dir.filter(p))
                                         else aux.uniao(esq.filter(p)).uniao(dir.filter(p))
      }
      loop(ConjVazio())
    }
    
    def map(f: Int => Int): ConjInt = {
      def loop(aux: ConjInt)= this match {
        case ConjVazio() => ConjVazio()
        case ConjCons(elem, esq, dir) => aux.insere(elem).uniao(esq.map(f)).uniao(dir.map(f))
      }
      loop(ConjVazio())
    }
    
    val arv = ConjCons(1, 
        ConjCons(2, ConjCons(4, ConjVazio(), ConjVazio()), ConjCons(5, ConjVazio(), ConjVazio())), 
        ConjCons(3, ConjCons(6, ConjVazio(), ConjVazio()), ConjVazio()))
      
    def flatMap(f: Int => ConjInt): ConjInt = ???
  }
  case class ConjVazio() extends ConjInt
  case class ConjCons(elem: Int, esq: ConjInt, dir: ConjInt) extends ConjInt
    
  def intersecao(c1: ConjInt, c2: ConjInt): ConjInt = ???
}