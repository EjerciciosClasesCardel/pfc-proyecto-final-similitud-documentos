package proyecto

import scala.util.Random
import scala.annotation.tailrec

/**
 * Detección de similitud entre documentos — versión secuencial.
 *
 * Cada documento se resume por el conjunto de sus fragmentos de k palabras
 * consecutivas, y dos documentos se comparan por el índice de Jaccard de esos
 * conjuntos. El índice invertido evita comparar las parejas que no comparten
 * ningún fragmento, cuya similitud es cero de antemano.
 *
 * Complete cada función. Las firmas y los tipos no se modifican.
 */
object Similitud {

  // Un fragmento es la secuencia de k palabras consecutivas.
  type Fragmento = Vector[String]

  // Un documento es una pareja con el id y sus palabras normalizadas.
  type Documento = (String, Vector[String])
  type Corpus = Vector[Documento]

  // La huella de un documento es el conjunto de sus fragmentos.
  type Huella = Set[Fragmento]

  // El indice invertido asocia a cada fragmento los documentos en los que
  // aparece, identificados por su posicion en el corpus.
  type Indice = Map[Fragmento, Set[Int]]

  // Una pareja de documentos, siempre con i < j, y una pareja medida.
  type Pareja = (Int, Int)
  type Medicion = (Int, Int, Double)

  // ---------------------------------------------------------------------------
  // Generación de entradas (ya implementadas, no se modifican)
  // ---------------------------------------------------------------------------

  val random = new Random()

  def documentoAlAzar(id: String, t: Int, v: Int): Documento =
    (id, Vector.fill(t)("p" + random.nextInt(v)))

  def corpusAlAzar(n: Int, t: Int, v: Int): Corpus =
    Vector.tabulate(n)(i => documentoAlAzar("D" + i, t, v))

  def copiaParcial(d: Documento, id: String, f: Double, v: Int): Documento = {
    val tomadas = (d._2.length * f).toInt
    (id, d._2.take(tomadas) ++
      Vector.fill(d._2.length - tomadas)("p" + random.nextInt(v)))
  }

  def corpusConCopias(n: Int, t: Int, v: Int, c: Int, f: Double): Corpus = {
    val base = corpusAlAzar(n, t, v)
    base ++ Vector.tabulate(c)(i => copiaParcial(base(i), "C" + i, f, v))
  }

  // ---------------------------------------------------------------------------
  // Exploración de entradas (ya implementadas, no se modifican)
  // ---------------------------------------------------------------------------

  def idDoc(d: Documento): String = d._1
  def palabrasDoc(d: Documento): Vector[String] = d._2
  def largoDoc(d: Documento): Int = d._2.length

  // ---------------------------------------------------------------------------
  // Aquí empieza su trabajo
  // ---------------------------------------------------------------------------

  /**
   * Pasa el texto a minúsculas, convierte en separador todo carácter que no
   * sea letra o dígito y devuelve el vector de palabras no vacías.
   *
   * Ejemplo: normalizar("!Hola, Mundo 2026!") == Vector("hola", "mundo", "2026")
   */
  def normalizar(texto: String): Vector[String] = {
    @tailrec
    def recorrer(cs: List[Char], actual: String, acc: Vector[String]) : Vector[String] = cs match {
      case Nil => if (actual.isEmpty) acc else acc :+ actual
      case c :: resto =>
        if (c.isLetterOrDigit) recorrer(resto, actual + c, acc)
        else if (actual.isEmpty) recorrer(resto, "", acc)
        else recorrer(resto, "", acc :+ actual)
    }
    recorrer(texto.toLowerCase.toList, "", Vector())
  }

  /**
   * Los t - k + 1 fragmentos de k palabras consecutivas, en el orden en que
   * aparecen; el vector vacío si el documento tiene menos de k palabras.
   *
   * Ejemplo: fragmentos(Vector("el", "gato", "duerme", "en"), 3) ==
   * Vector(Vector("el", "gato", "duerme"), Vector("gato", "duerme", "en"))
   */
  def fragmentos(ts: Vector[String], k: Int): Vector[Fragmento] = {
    if (ts.length < k) Vector()
    else ts.take(k) +: fragmentos(ts.tail, k)
  }

  /** El conjunto de fragmentos del documento: un repetido cuenta una vez. */
  def huella(d: Documento, k: Int): Huella = ???

  /** Una huella por documento del corpus, en el mismo orden. */
  def huellas(c: Corpus, k: Int): Vector[Huella] = ???

  /**
   * Índice de Jaccard de dos huellas, el tamaño de la intersección sobre el de
   * la unión. Dos huellas vacías dan cero y no dividen por cero.
   *
   * Ejemplo: jaccard(Set(Vector("a", Vector("b")), Set(Vector("b"), Vector("c"))) == 1.0 / 3.0
   */
  def jaccard(h1: Huella, h2: Huella): Double = {
    val comunes = (h1 intersect h2).size
    val total = h1.size + h2.size - comunes
    if (total == 0) 0.0 else comunes.toDouble / total
  }

  /** Las n (n - 1) / 2 parejas (i, j) con i < j, en orden. */
  def parejas(n: Int): Vector[Pareja] = ???

  /** La similitud de cada pareja que se le pase, en el mismo orden. */
  def similitudes(hs: Vector[Huella], ps: Vector[Pareja]): Vector[Medicion] = ???

  /**
   * El mapa de cada fragmento al conjunto de documentos donde aparece, con los
   * documentos identificados por su posición en el vector de huellas.
   */
  def indiceInvertido(hs: Vector[Huella]): Indice = ???

  /**
   * Las parejas que comparten al menos un fragmento, sin repetir y ordenadas.
   * Una misma pareja puede salir de varias entradas del índice.
   */
  def parejasCandidatas(idx: Indice): Vector[Pareja] = ???

  /**
   * Las mediciones que alcanzan el umbral, de mayor a menor similitud y
   * truncadas a p. Los empates se rompen por posición, de modo que el
   * resultado no dependa del orden en que llegaron las mediciones.
   */
  def ranking(ss: Vector[Medicion], umbral: Double, p: Int): Vector[Medicion] = ???

  /** Solución directa: compara las n (n - 1) / 2 parejas. */
  def detectarDirecto(c: Corpus, k: Int, umbral: Double,
                      p: Int): Vector[Medicion] = ???

  /** Solución con índice: compara solo las parejas candidatas. */
  def detectarConIndice(c: Corpus, k: Int, umbral: Double,
                        p: Int): Vector[Medicion] = ???
}
