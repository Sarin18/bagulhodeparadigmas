//exercicio 1
object TamanhoLista {

  //função
  def tamanhoFuncao[T](lista: List[T]): Int = lista match {
    case Nil => 0
    case _ :: tail => 1 + tamanho_func(tail) //aqui ele incrementa e chama a função com o restante da lista
  }

  def main(args: Array[String]): Unit = {
    val listaBase = List(1, 2, 3, 4, 5)
    val tamanho = tamanhoFuncao(listaBase)
    println(s"O tamanho da lista é: $tamanho")
  }
}
//----------------------------------------------------------------------------------------------------------------//

//exercicio 2
object ChecagemDeElemento {

  def contarOcorrencias[T](lista: List[T], elemento: T): Int = lista match {
    case Nil => 0 // Caso base: lista vazia
    case head :: tail =>
      if (head == elemento) 1 + contarOcorrencias(tail, elemento)
      else contarOcorrencias(tail, elemento)
  }

  //verifica se o elemento aparece exatamente uma vez na lista
  def isUnique[T](lista: List[T], elemento: T): Boolean = {
    contarOcorrencias(lista, elemento) == 1
  }

  def main(args: Array[String]): Unit = {
    val listaBase = List(1, 2, 3, 4, 5, 2)
    val elemento = 2
    val resultado = isUnique(listaBase, elemento)
    println(s"O elemento $elemento aparece uma única vez na lista? $resultado")
  }
}
//----------------------------------------------------------------------------------------------------------------//

//exercicio 3
object Soma {

  def somaDeElementos(lista: List[Int], condicao: Int => Boolean): Int = lista match {
    case Nil => 0
    case head :: tail =>
      if (condicao(head)) head + somaDeElementos(tail, condicao) //soma se atender a condiçao
      else somaDeElementos(tail, condicao)
  }

  def main(args: Array[String]): Unit = {
    val listaBase = List(1, 2, 3, 4, 5, 6)
    val condicao = (x: Int) => x % 2 == 0
    val resultado = somaDeElementos(listaBase, condicao)
    println(s"A soma dos elementos é: $resultado")
  }
}
//----------------------------------------------------------------------------------------------------------------//

//exercicio 4
object MultiplicacaoDeMatrizes {

  def podeMultiplicar(a: Array[Array[Int]], b: Array[Array[Int]]): Boolean = {
    a.headOption.exists(_.length == b.length)
  }

  def multiplicarMatrizes(a: Array[Array[Int]], b: Array[Array[Int]]): Array[Array[Int]] = {
    if (!podeMultiplicar(a, b)) {
      throw new IllegalArgumentException("não podem ser multiplicadas por causa das suas dimensões diferentes.")
    }

    val linhaA = a.length
    val colunaA = a(0).length
    val colunaB = b(0).length

    // Cria a matriz resultante com dimensões adequadas
    val resultado = Array.ofDim[Int](linhaA, colunaB)

    for (i <- 0 until linhaA) {
      for (j <- 0 until colunaB) {
        var soma = 0
        for (k <- 0 until colunaA) {
          soma += a(i)(k) * b(k)(j)
        }
        resultado(i)(j) = soma
      }
    }

    resultado
  }

  def main(args: Array[String]): Unit = {
    val a = Array(
      Array(1, 2, 3),
      Array(4, 5, 6)
    )

    val b = Array(
      Array(7, 8),
      Array(9, 10),
      Array(11, 12)
    )

    try {
      val resultado = multiplicarMatrizes(a, b)
      println("O resultado da multiplicação das matrizes é:")
      resultado.foreach(linha => println(linha.mkString(" ")))
    } catch {
      case e: IllegalArgumentException => println(e.getMessage)
    }
  }
}
//----------------------------------------------------------------------------------------------------------------//

//exercicio 5
object RemoverOcorrencias {

  def removeOcorrencia(s1: String, s2: String): String = {
    s1.replace(s2, "")
  }

  def main(args: Array[String]): Unit = {
    val frase1 = ("paralelepipedo", "le")
    val frase2 = ("parei de ver o rei.", "rei")

    val resultado1 = removeOcorrencia(frase1._1, frase1._2)
    val resultado2 = removeOcorrencia(frase2._1, frase2._2)

    println(s"Resultado para '${frase1._1}' sem '${frase1._2}': $resultado1")
    println(s"Resultado para '${frase2._1}' sem '${frase2._2}': $resultado2")
  }
}
//----------------------------------------------------------------------------------------------------------------//

//exercicio 6
object NomesAntesDoNome {

  def nomesAnteriores(lista: List[String], nome: String): List[String] = {
    val listaOrdenada = lista.sorted //ordena a lista em ordem alfabética
    listaOrdenada.filter(_ < nome) //filtra os nomes que vem antes da referencia
  }

  def main(args: Array[String]): Unit = {
    val listaNomes = List("Fernando", "Ana", "Eduarda", "Carlos", "Bruno", "Daniel")
    val nomeReferencia = "Daniel"

    val resultado = nomesAnteriores(listaNomes, nomeReferencia)
    println(s"Nomes na lista que vêm antes de '$nomeReferencia' em ordem alfabética: ${resultado.mkString(", ")}")
  }
}
//----------------------------------------------------------------------------------------------------------------//

//exercicio 7
object UniaoDeListas {

  def uneListas[T](listaBase1: List[T], listaBase2: List[T]): List[T] = {
    (listaBase1 ++ listaBase2).distinct
  }

  def main(args: Array[String]): Unit = {
    val listaBase1 = List(1, 2, 3, 4, 5)
    val listaBase2 = List(4, 5, 6, 7, 8)

    val resultado = uneListas(listaBase1, listaBase2)
    println(s"A união das duas listas é: ${resultado.mkString(", ")}")
  }
}
//----------------------------------------------------------------------------------------------------------------//

//exercicio 8
object Anagramas {

  def verificaSeSaoAnagramas(str1: String, str2: String): Boolean = {
    
    val s1 = str1.replaceAll("\\s", "").toLowerCase//remove espaços e converte para minusculas
    val s2 = str2.replaceAll("\\s", "").toLowerCase//remove espaços e converte para minusculas
    
    //verifica se as strings têm o mesmo taamnho depois da normalizaçao
    if (s1.length != s2.length) {
      false
    } else {
    
      s1.sorted == s2.sorted //ordena os caracteres das strings e compara
    }
  }

  def main(args: Array[String]): Unit = {
    val string1 = "paralelepípedo"
    val string2 = "piparapeleledo"

    val resultado = verificaSeSaoAnagramas(string1, string2)
    println(s"As strings '$string1' e '$string2' são anagramas? $resultado")

    val string3 = "escuta"
    val string4 = "acutes"

    val resultado2 = verificaSeSaoAnagramas(string3, string4)
    println(s"As strings '$string3' e '$string4' são anagramas? $resultado2")
  }
}

//----------------------------------------------------------------------------------------------------------------//

//exercicio 9
object VerificarNumeroPrimo {

  def verificaPrimo(numero: Int): Boolean = {
    
    if (numero <= 1) { //casos especiais: 0, 1 e negativos nao sao primos
      false
    } 
    else {
      //verifica se é divisivel por todos os numeros de 2 ate a raiz quadrada do numero
      val limite = Math.sqrt(numero).toInt
      def iterador(divisor: Int): Boolean = {
        if (divisor > limite) {
          true //é primo
        } else if (numero % divisor == 0) {
          false //nao é primo
        } else {
          iterador(divisor + 1)
        }
      }
      iterador(2)
    }
  }

  def main(args: Array[String]): Unit = {
    val numeros = List(2, 3, 4, 5, 6, 7, 8, 9, 10)

    numeros.foreach(numero => {
      val resultado = verificaPrimo(numero)
      println(s"O número $numero é primo? $resultado")
    })
  }
}
//----------------------------------------------------------------------------------------------------------------//

//exercicio 10
object InteiroParaHexadecimal {

  def paraHexadecimal(numero: Int): String = {
    
    numero.toHexString //a linguagem tem o metodo toHexString para conversao para hexadecimal
  }

  def main(args: Array[String]): Unit = {
    val numeros = List(10, 255, 1000, -1)

    numeros.foreach(numero => {
      val hexadecimal = paraHexadecimal(numero)
      println(s"O número $numero em hexadecimal é: $hexadecimal")
    })
  }
}
//----------------------------------------------------------------------------------------------------------------//

//exercicio 11
object ConverterCaixaAlta {

  def CaixaAlta(str: String): String = {
    
    str.toUpperCase //usa o toUpperCase para converter em maiusculas
  }

  def main(args: Array[String]): Unit = {
    val stringExemplo = "i am the bone of my sword"

    val resultado = CaixaAlta(stringExemplo)
    println(s"A string '$stringExemplo' em caixa alta é: $resultado")
  }
}

//----------------------------------------------------------------------------------------------------------------//

//exercicio 12
