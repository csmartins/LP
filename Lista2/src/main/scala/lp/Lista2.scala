package lp

object Lista2 extends App {
  // escreva testes de suas funções aqui
  println("Olá, Mundo!")
  
  val pares: Conjunto[Int] = (x: Int) => (x % 2) == 0
  val impares: Conjunto[Int] = (x: Int) => (x % 2) == 1
  val multiplos3: Conjunto[Int] = (x: Int) => (x % 3) == 0
  
  println(contem(pares, 1))
  println(contem(pares, 6))
  
  println(contem(unitario(1), 3))
  println(contem(unitario(1), 1))
  
  //uniao(pares, impares)
}
