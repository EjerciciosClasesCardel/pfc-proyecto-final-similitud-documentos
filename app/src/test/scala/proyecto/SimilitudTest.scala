package proyecto

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import Similitud._

/**
 * Pruebas de contrato de la versión secuencial: una por función, con los
 * valores de los dos ejemplos del enunciado. No se modifican.
 *
 * La rúbrica pide además cinco casos propios por función, distintos de estos y
 * de los ejemplos del enunciado. Van al final de este archivo o en un archivo
 * nuevo de esta misma carpeta; las dos formas valen.
 */
@RunWith(classOf[JUnitRunner])
class SimilitudTest extends AnyFunSuite {

  val eps = 1e-9

  // Ejemplo 1 del enunciado, con las palabras ya normalizadas para que estas
  // pruebas no dependan unas de otras.
  val d0: Documento = ("d0", Vector("el", "gato", "duerme", "en", "el", "sofa"))
  val d1: Documento = ("d1", Vector("el", "gato", "duerme", "en", "la", "alfombra"))
  val d2: Documento = ("d2", Vector("la", "lluvia", "moja", "el", "parque"))
  val c1: Corpus = Vector(d0, d1, d2)

  // Ejemplo 2 del enunciado.
  val e0: Documento = ("e0", Vector("la", "funcion", "recursiva", "termina",
    "cuando", "llega", "al", "caso", "base"))
  val e1: Documento = ("e1", Vector("toda", "funcion", "recursiva", "termina",
    "cuando", "llega", "al", "caso", "base"))
  val e2: Documento = ("e2", Vector("el", "paralelismo", "divide", "el",
    "trabajo", "en", "tareas", "independientes"))
  val e3: Documento = ("e3", Vector("el", "paralelismo", "divide", "el",
    "trabajo", "en", "hilos", "independientes"))
  val c2: Corpus = Vector(e0, e1, e2, e3)

  test("normalizar: baja a minúsculas y parte en palabras") {
    assert(normalizar("El Gato Duerme") == Vector("el", "gato", "duerme"))
  }

  test("fragmentos: son las ventanas consecutivas, en orden") {
    val ts = Vector("a", "b", "c", "d")
    assert(fragmentos(ts, 2) ==
      Vector(Vector("a", "b"), Vector("b", "c"), Vector("c", "d")))
  }

  test("huella: el ejemplo 1 tiene 4, 4 y 3 fragmentos") {
    assert(huella(d0, 3).size == 4)
    assert(huella(d1, 3).size == 4)
    assert(huella(d2, 3).size == 3)
  }

  test("huellas: hay una huella por documento") {
    assert(huellas(c1, 3).length == 3)
  }

  test("jaccard: ejemplo 2, las dos parejas dan 0.75 y 0.5") {
    assert(math.abs(jaccard(huella(e0, 3), huella(e1, 3)) - 0.75) < eps)
    assert(math.abs(jaccard(huella(e2, 3), huella(e3, 3)) - 0.5) < eps)
  }

  test("parejas: hay n (n - 1) / 2") {
    assert(parejas(5).length == 10)
  }

  test("similitudes: el ejemplo 1 da 1/3, 0 y 0") {
    val hs = huellas(c1, 3)
    val ms = similitudes(hs, parejas(3)).map(_._3)
    assert(math.abs(ms(0) - 2.0 / 6.0) < eps && ms(1) == 0.0 && ms(2) == 0.0)
  }

  test("indiceInvertido: el fragmento inicial de d0 está en d0 y en d1") {
    val idx = indiceInvertido(huellas(c1, 3))
    assert(idx(Vector("el", "gato", "duerme")) == Set(0, 1))
  }

  test("parejasCandidatas: en el ejemplo 2 quedan dos de seis") {
    val idx = indiceInvertido(huellas(c2, 3))
    assert(parejasCandidatas(idx) == Vector((0, 1), (2, 3)))
  }

  test("ranking: ordena de mayor a menor y trunca a p") {
    val ms: Vector[Medicion] = Vector((0, 1, 0.4), (0, 2, 0.1), (1, 2, 0.9))
    assert(ranking(ms, 0.0, 10).map(_._3) == Vector(0.9, 0.4, 0.1))
    assert(ranking(ms, 0.0, 2).length == 2)
  }

  test("detectar: el ejemplo 2 reporta (0, 1) con 0.75 y (2, 3) con 0.5") {
    val r = detectarDirecto(c2, 3, 0.3, 5)
    assert(r.map(m => (m._1, m._2)) == Vector((0, 1), (2, 3)))
    assert(math.abs(r(0)._3 - 0.75) < eps && math.abs(r(1)._3 - 0.5) < eps)
  }

  test("detectar: las dos soluciones coinciden en los ejemplos") {
    assert(detectarDirecto(c1, 3, 0.2, 5) == detectarConIndice(c1, 3, 0.2, 5))
    assert(detectarDirecto(c2, 3, 0.3, 5) == detectarConIndice(c2, 3, 0.3, 5))
  }

  // ===========================================================================
  // Integrante 2: jaccard, parejas, similitudes
  // ===========================================================================

  /** Una huella a partir de fragmentos escritos como "a b c". */
  def i2hu(fs: String*): Huella = fs.map(_.split(" ").toVector).toSet

  /** Huella calculada sin usar las funciones de otros integrantes. */
  def i2HuellaLocal(d: Documento, k: Int): Huella =
    d._2.sliding(k).filter(_.length == k).map(_.toVector).toSet

  def i2HuellasLocales(c: Corpus, k: Int): Vector[Huella] =
    c.map(i2HuellaLocal(_, k))

  // --- jaccard ---------------------------------------------------------------

  test("jaccard: huellas disjuntas dan 0") {
    assert(jaccard(i2hu("a b", "b c"), i2hu("x y", "y z")) == 0.0)
  }

  test("jaccard: huellas iguales dan 1") {
    val h = i2hu("a b", "b c", "c d")
    assert(jaccard(h, h) == 1.0)
  }

  test("jaccard: dos huellas vacías dan 0 y no NaN") {
    val r = jaccard(Set(), Set())
    assert(r == 0.0 && !r.isNaN)
  }

  test("jaccard: una huella vacía y otra no dan 0") {
    assert(jaccard(Set(), i2hu("a b")) == 0.0)
    assert(jaccard(i2hu("a b"), Set()) == 0.0)
  }

  test("jaccard: intersección 2 y unión 5 dan 0.4") {
    val a = i2hu("a b", "b c", "c d")
    val b = i2hu("b c", "c d", "d e", "e f")
    assert(math.abs(jaccard(a, b) - 0.4) < eps)
  }

  test("jaccard: una huella contenida en otra da |A| / |B|") {
    val a = i2hu("a b", "b c")
    val b = i2hu("a b", "b c", "c d", "d e")
    assert(math.abs(jaccard(a, b) - 0.5) < eps)
  }

  test("jaccard: es simétrica y el orden de los fragmentos no cuenta") {
    val a = i2hu("a b", "b c", "c d")
    val b = i2hu("c d", "z z", "a b")
    assert(jaccard(a, b) == jaccard(b, a))
    assert(math.abs(jaccard(a, b) - 0.5) < eps)
  }

  test("jaccard: un fragmento con las mismas palabras en otro orden es distinto") {
    assert(jaccard(i2hu("a b"), i2hu("b a")) == 0.0)
  }

  test("jaccard: queda en [0, 1] y cumple la definición en corpus al azar") {
    val hs = i2HuellasLocales(corpusAlAzar(20, 80, 10), 2)
    val todas = for { a <- hs; b <- hs } yield (a, b)
    assert(todas.forall { case (a, b) =>
      val j = jaccard(a, b)
      val esperado = a.intersect(b).size.toDouble / a.union(b).size
      j >= 0.0 && j <= 1.0 && math.abs(j - esperado) < eps
    })
  }

  // --- parejas ---------------------------------------------------------------

  test("parejas: con 0 y con 1 documentos no hay parejas") {
    assert(parejas(0) == Vector())
    assert(parejas(1) == Vector())
  }

  test("parejas: un número negativo de documentos da el vector vacío") {
    assert(parejas(-3) == Vector())
  }

  test("parejas: con 2 documentos solo está (0, 1)") {
    assert(parejas(2) == Vector((0, 1)))
  }

  test("parejas: con 4 documentos salen las seis, en orden") {
    assert(parejas(4) ==
      Vector((0, 1), (0, 2), (0, 3), (1, 2), (1, 3), (2, 3)))
  }

  test("parejas: hay n (n - 1) / 2 para n de 0 a 40") {
    assert((0 to 40).forall(n => parejas(n).length == n * (n - 1) / 2))
  }

  test("parejas: todas cumplen 0 <= i < j < n y ninguna se repite") {
    val n = 25
    val ps = parejas(n)
    assert(ps.forall { case (i, j) => 0 <= i && i < j && j < n })
    assert(ps.distinct.length == ps.length)
  }

  test("parejas: salen en orden lexicográfico") {
    val ps = parejas(15)
    assert(ps == ps.sorted)
  }

  test("parejas: el último es (n - 2, n - 1) y el primero (0, 1)") {
    assert(parejas(9).head == ((0, 1)) && parejas(9).last == ((7, 8)))
  }

  // --- similitudes -----------------------------------------------------------

  val i2h0: Huella = i2hu("a b", "b c", "c d")
  val i2h1: Huella = i2hu("b c", "c d", "d e", "e f")
  val i2h2: Huella = i2hu("x y")
  val i2hv: Vector[Huella] = Vector(i2h0, i2h1, i2h2)

  test("similitudes: sin parejas da el vector vacío") {
    assert(similitudes(i2hv, Vector()) == Vector())
  }

  test("similitudes: valores conocidos de las tres parejas") {
    val ms = similitudes(i2hv, parejas(3))
    assert(ms.map(m => (m._1, m._2)) == Vector((0, 1), (0, 2), (1, 2)))
    assert(math.abs(ms(0)._3 - 0.4) < eps)
    assert(ms(1)._3 == 0.0 && ms(2)._3 == 0.0)
  }

  test("similitudes: conserva el orden de las parejas que se le pasan") {
    val ps: Vector[Pareja] = Vector((1, 2), (0, 1), (0, 2))
    val ms = similitudes(i2hv, ps)
    assert(ms.map(m => (m._1, m._2)) == ps)
    assert(ms(0)._3 == 0.0 && math.abs(ms(1)._3 - 0.4) < eps)
  }

  test("similitudes: una sola pareja") {
    val ms = similitudes(i2hv, Vector((0, 1)))
    assert(ms.length == 1 && math.abs(ms(0)._3 - 0.4) < eps)
  }

  test("similitudes: una huella vacía da 0 con todas") {
    val vacia: Huella = Set()
    val ms = similitudes(Vector(i2h0, vacia, i2h1), parejas(3))
    assert(ms(0)._3 == 0.0 && ms(2)._3 == 0.0)
    assert(!ms.exists(_._3.isNaN))
  }

  test("similitudes: cada valor coincide con jaccard de sus huellas") {
    val hs = i2HuellasLocales(corpusAlAzar(25, 60, 8), 2)
    val ms = similitudes(hs, parejas(hs.length))
    assert(ms.length == hs.length * (hs.length - 1) / 2)
    assert(ms.forall { case (i, j, s) => s == jaccard(hs(i), hs(j)) })
  }

  test("similitudes: un documento copiado a medias queda muy por encima del azar") {
    val c = corpusConCopias(10, 200, 50, 1, 0.5)
    val hs = i2HuellasLocales(c, 3)
    val ms = similitudes(hs, parejas(c.length))
    val mejor = ms.maxBy(_._3)
    assert((mejor._1, mejor._2) == ((0, 10)))
  }
}
