package proyecto

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import Similitud._
import SimilitudPar._

/**
 * Pruebas de contrato de la versión paralela: cada una compara contra la
 * versión secuencial, que es la condición que pide el enunciado. No se
 * modifican.
 */
@RunWith(classOf[JUnitRunner])
class SimilitudParTest extends AnyFunSuite {

  val c1: Corpus = Vector(
    ("d0", Vector("el", "gato", "duerme", "en", "el", "sofa")),
    ("d1", Vector("el", "gato", "duerme", "en", "la", "alfombra")),
    ("d2", Vector("la", "lluvia", "moja", "el", "parque")))

  val c2: Corpus = Vector(
    ("e0", Vector("la", "funcion", "recursiva", "termina", "cuando", "llega",
      "al", "caso", "base")),
    ("e1", Vector("toda", "funcion", "recursiva", "termina", "cuando", "llega",
      "al", "caso", "base")),
    ("e2", Vector("el", "paralelismo", "divide", "el", "trabajo", "en",
      "tareas", "independientes")),
    ("e3", Vector("el", "paralelismo", "divide", "el", "trabajo", "en",
      "hilos", "independientes")))

  test("huellasPar: da lo mismo que la secuencial") {
    assert(huellasPar(c1, 3) == huellas(c1, 3))
    assert(huellasPar(c2, 3) == huellas(c2, 3))
  }

  test("similitudesPar: da lo mismo que la secuencial, y conserva el orden") {
    val hs = huellas(c2, 3)
    val ps = parejas(c2.length)
    assert(similitudesPar(hs, ps, 1) == similitudes(hs, ps))
    assert(similitudesPar(hs, ps, 100) == similitudes(hs, ps))
  }

  test("fusionar: une los conjuntos del fragmento que está en los dos") {
    val g: Fragmento = Vector("a", "b")
    val h: Fragmento = Vector("c", "d")
    val i1: Indice = Map(g -> Set(0), h -> Set(1))
    val i2: Indice = Map(g -> Set(2))
    assert(fusionar(i1, i2) == Map(g -> Set(0, 2), h -> Set(1)))
    assert(fusionar(i1, Map()) == i1)
  }

  test("indiceInvertidoPar: da lo mismo que la secuencial en los ejemplos") {
    assert(indiceInvertidoPar(huellas(c1, 3)) == indiceInvertido(huellas(c1, 3)))
    assert(indiceInvertidoPar(huellas(c2, 3)) == indiceInvertido(huellas(c2, 3)))
  }

  test("detectar: las cuatro versiones coinciden") {
    val directo = detectarDirecto(c2, 3, 0.3, 5)
    assert(detectarConIndice(c2, 3, 0.3, 5) == directo)
    assert(detectarDirectoPar(c2, 3, 0.3, 5, 2) == directo)
    assert(detectarConIndicePar(c2, 3, 0.3, 5, 2) == directo)
  }

  // ===========================================================================
  // Integrante 2: similitudesPar
  // ===========================================================================

  def i2hu(fs: String*): Huella = fs.map(_.split(" ").toVector).toSet

  def i2HuellasLocales(c: Corpus, k: Int): Vector[Huella] =
    c.map(_._2.sliding(k).filter(_.length == k).map(_.toVector).toSet)

  val i2hv: Vector[Huella] = Vector(
    i2hu("a b", "b c", "c d"),
    i2hu("b c", "c d", "d e", "e f"),
    i2hu("x y"))
  val i2hsAzar: Vector[Huella] = i2HuellasLocales(corpusAlAzar(30, 120, 15), 2)
  val i2psAzar: Vector[Pareja] = parejas(i2hsAzar.length)

  test("similitudesPar: da lo mismo que similitudes para varios cortes") {
    val esperado = similitudes(i2hsAzar, i2psAzar)
    assert(Vector(1, 2, 3, 7, 50, 128, 435, 10000).forall(corte =>
      similitudesPar(i2hsAzar, i2psAzar, corte) == esperado))
  }

  test("similitudesPar: conserva el orden de las parejas") {
    val ps: Vector[Pareja] = i2psAzar.reverse
    val r = similitudesPar(i2hsAzar, ps, 16)
    assert(r.map(m => (m._1, m._2)) == ps)
  }

  test("similitudesPar: sin parejas da el vector vacío") {
    assert(similitudesPar(i2hv, Vector(), 4) == Vector())
    assert(similitudesPar(i2hv, Vector(), 0) == Vector())
  }

  test("similitudesPar: una sola pareja, con cualquier corte") {
    val esperado = similitudes(i2hv, Vector((0, 1)))
    assert(similitudesPar(i2hv, Vector((0, 1)), 1) == esperado)
    assert(similitudesPar(i2hv, Vector((0, 1)), 0) == esperado)
    assert(similitudesPar(i2hv, Vector((0, 1)), 1000) == esperado)
  }

  test("similitudesPar: un corte cero o negativo termina y da el mismo resultado") {
    val esperado = similitudes(i2hsAzar, i2psAzar)
    assert(similitudesPar(i2hsAzar, i2psAzar, 0) == esperado)
    assert(similitudesPar(i2hsAzar, i2psAzar, -5) == esperado)
  }

  test("similitudesPar: devuelve una medición por pareja") {
    assert(similitudesPar(i2hsAzar, i2psAzar, 20).length == i2psAzar.length)
  }

  test("similitudesPar: corpus mayor, con el corte del enunciado") {
    val hs = i2HuellasLocales(corpusAlAzar(80, 300, 40), 4)
    val ps = parejas(hs.length)
    assert(similitudesPar(hs, ps, 128) == similitudes(hs, ps))
  }

  test("similitudesPar: con un corte de 2 sale lo mismo en los ejemplos a mano") {
    assert(similitudesPar(i2hv, parejas(3), 2) == similitudes(i2hv, parejas(3)))
  }
}
