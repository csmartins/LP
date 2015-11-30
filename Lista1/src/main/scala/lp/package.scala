import scala.annotation.tailrec
package object lp {
  def pascal(col: Int, lin: Int): Int = {
    if (col == 0 || col == lin) 1 else pascal(col, lin - 1) + pascal(col - 1, lin - 1)
  }

  def balanceado(l: List[Char]): Boolean = {
    @tailrec
    def balanceamento(l: List[Char], abertos: Int): Boolean = {
      if (l.isEmpty)
        if (abertos == 0) true else false
      else if (l.head == '(') {
        balanceamento(l.tail, abertos + 1)
      } else if (l.head == ')') {
        if (abertos > 0) {
          balanceamento(l.tail, abertos - 1)
        } else
          false
      } else balanceamento(l.tail, abertos)
    }
    balanceamento(l, 0)
  }
  
  def particao(l: List[Int], pivo: Int): (List[Int], List[Int]) = {
    @tailrec
    def loop(menores: List[Int], maiores: List[Int], l: List[Int]): (List[Int], List[Int]) = {
      if(l.isEmpty)
        (menores, maiores)
      else if(l.head <= pivo)
        loop(menores.::(l.head), maiores, l.tail)
      else 
        loop(menores, maiores.::(l.head), l.tail)
    }
    loop(List(), List(), l)
  }

  def quicksort(l: List[Int]): List[Int] = 
  {
    def isSorted(l: List[Int]): Boolean = {
      if(l.size == 1)
        true
      else if(!l.isEmpty)
        (l.head <= l.tail.head) && isSorted(l.tail)
      else
        true
    }
    
    if(!l.isEmpty)
    {
    	val pivo = l.head
 			var (menores, maiores) = particao(l, pivo)
 			if(!isSorted(menores))
 			{
 				quicksort(menores).++(maiores)
 			  
 			}
 			else if(!isSorted(maiores))
 			{
				menores.++(quicksort(maiores))
 			}
 			else
 			  menores.++(maiores)
    }
    else
      Nil
  }
  
  def crSemestre(notas: List[(Double, Int)]): (Double, Int) = 
  {
    def calcularNumerador(notas: List[(Double, Int)]): Double = 
    {
      if(!notas.isEmpty)
        notas.head._1*notas.head._2 + calcularNumerador(notas.tail)
      else
        0
    }
    def calcularDenominador(notas: List[(Double, Int)]): Int =
    {
      if(!notas.isEmpty)
        notas.head._2 + calcularDenominador(notas.tail)
      else
        0
    }
    
    (calcularNumerador(notas)/calcularDenominador(notas), calcularDenominador(notas))   
  }
  
  def crsAcumulados(semestres: List[List[(Double, Int)]]): (List[Double], Int) = 
  {
    def somarCrs(crs: List[Double], soma: Double, result: List[Double]): List[Double] = 
    {
      if(!crs.isEmpty)
        somarCrs(crs.tail, soma + crs.head, result.++(List(soma + crs.head)))
      else
        result    
    }
    if(!semestres.isEmpty)
    {
      val listCrsAcumulados = List(crSemestre(semestres.head)._1).++(crsAcumulados(semestres.tail)._1) 
      val creditos =	crSemestre(semestres.head)._2 + crsAcumulados(semestres.tail)._2
      
      (somarCrs(listCrsAcumulados, 0, List()), creditos)
    }
    else
      (Nil, 0)
  }

}