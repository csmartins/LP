import parser._

package object lp {
  trait CL
  case class Var(nome: String) extends CL
  case class Ap(funcao: CL, argumento: CL) extends CL
  case class Abs(parametro: String, corpo: CL) extends CL
  
  //EXP  -> AEXP {AEXP}
  def ExpCL: Parser[CL] = chainl(AExpCL, empty(Ap), AExpCL)
  
  //AEXP -> id | '\' id '.' EXP | '(' EXP ')'
  def AExpCL: Parser[CL] = (for {
    (nome, _) <- id
  }yield Var(nome)) +: (for {
    _ <- op("(")
    e <- ExpCL
    _ <- op(")")
  }yield e) +: (for {
    _ <- op("\\")
    (param, _) <- id
    _ <- op(".")
    corpo <- ExpCL
  }yield Abs(param, corpo))
  
  //avaliacao sem checagem de variaveis livres
  def eval(e: CL): Abs = e match {
    //o valor de uma abstração é ela mesma;
    case Abs(param, corpo) => Abs(param, corpo)
    //para obter o valor de uma aplicação
    case Ap(fun, arg) => {
      
      //obtemos o valor do seu lado esquerdo (que deverá ser uma abstração, ou a aplicação é indefinida)
      val Abs(param, corpo) = eval(fun)
      
      //e seu lado direito
      val varg = eval(arg)
      
      //substituimos o parametro do lado esquerdo pelo valor do lado direito no corpo e avaliamos o resultado
      eval(subst(param, varg, corpo))
    }
  }
  
  def subst(oque: String, peloque: CL, onde: CL): CL = onde match {
    case Var(nome) => if(nome == oque) peloque else Var(nome)
    case Abs(param, corpo) => if(param == oque) Abs(param, corpo)
                              else Abs(param, subst(oque, peloque, corpo))
    case Ap(fun, arg) => Ap(subst(oque, peloque, fun), subst(oque, peloque, arg))                             
  }
  
  def step(e: CL): CL = e match {
    case Ap(Abs(param, corpo), Abs(p, c)) => subst(param, Abs(p,c), corpo)
    case Ap(Abs(param, corpo), a) => Ap(Abs(param, corpo), step(a))
    case Ap(f, a) => Ap(step(f), a)
  }
  
  def eval_cbn(e: CL): Abs = e match {
    case Abs(param, corpo) => Abs(param, corpo)
    case Ap(fun, arg) => {
      val Abs(param, corpo) = eval(fun)
      eval(subst(param, arg, corpo))
    }
  }
  
  def step_cbn(e: CL): CL = e match {
    case Ap(Abs(param, corpo), a) => subst(param, a, corpo)
    case Ap(f, a) => Ap(step(f), a)
  }
  
}
