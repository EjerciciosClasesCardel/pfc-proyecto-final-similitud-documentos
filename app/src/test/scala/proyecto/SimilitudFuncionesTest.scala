package proyecto

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import Similitud._

@RunWith(classOf[JUnitRunner])
class SimilitudFuncionesTest extends AnyFunSuite {

  test("similitudes calcula el Jaccard de todas las parejas en su orden") {
    val hs = Vector(
      Set(Vector("a"), Vector("b")),
      Set(Vector("a"), Vector("b"), Vector("c")),
      Set(Vector("z"))
    )
    val resultado = similitudes(hs, Vector((0, 1), (0, 2), (1, 2)))
    assert(resultado.map(_._3) == Vector(2.0 / 3.0, 0.0, 0.0))
  }

  test("similitudes respeta el orden recibido aunque no sea el orden natural") {
    val hs = Vector(Set(Vector("x")), Set(Vector("x")), Set(Vector("y")))
    assert(similitudes(hs, Vector((1, 2), (0, 1))) ==
      Vector((1, 2, 0.0), (0, 1, 1.0)))
  }

  test("similitudes devuelve uno para huellas idénticas") {
    val huella = Set(Vector("a", "b"))
    assert(similitudes(Vector(huella, huella), Vector((0, 1))) ==
      Vector((0, 1, 1.0)))
  }

  test("similitudes devuelve cero para dos huellas vacías") {
    val vacia: Huella = Set.empty
    assert(similitudes(Vector(vacia, vacia), Vector((0, 1))) ==
      Vector((0, 1, 0.0)))
  }

  test("similitudes devuelve un vector vacío cuando no recibe parejas") {
    assert(similitudes(Vector(Set(Vector("a"))), Vector.empty).isEmpty)
  }

  test("indiceInvertido asocia cada fragmento con sus posiciones") {
    val hs = Vector(
      Set(Vector("a"), Vector("b")),
      Set(Vector("a"), Vector("c"))
    )
    assert(indiceInvertido(hs) ==
      Map(Vector("a") -> Set(0, 1), Vector("b") -> Set(0), Vector("c") -> Set(1)))
  }

  test("indiceInvertido de un corpus vacío es vacío") {
    assert(indiceInvertido(Vector.empty).isEmpty)
  }

  test("indiceInvertido ignora huellas vacías sin perder las otras posiciones") {
    val hs = Vector(Set(Vector("a")), Set.empty[Fragmento], Set(Vector("b")))
    assert(indiceInvertido(hs) ==
      Map(Vector("a") -> Set(0), Vector("b") -> Set(2)))
  }

  test("indiceInvertido registra un fragmento compartido en todos sus documentos") {
    val fragmento = Vector("x", "y")
    val hs = Vector(Set(fragmento), Set(fragmento), Set(fragmento))
    assert(indiceInvertido(hs) == Map(fragmento -> Set(0, 1, 2)))
  }

  test("indiceInvertido conserva fragmentos exclusivos además de los compartidos") {
    val compartido = Vector("comun")
    val exclusivo = Vector("unico")
    val hs = Vector(Set(compartido, exclusivo), Set(compartido))
    val indice = indiceInvertido(hs)
    assert(indice(compartido) == Set(0, 1))
    assert(indice(exclusivo) == Set(0))
  }

  test("parejasCandidatas genera combinaciones para dos grupos independientes") {
    val indice: Indice = Map(
      Vector("a") -> Set(0, 3, 5),
      Vector("b") -> Set(1, 2, 4)
    )
    assert(parejasCandidatas(indice) ==
      Vector((0, 3), (0, 5), (1, 2), (1, 4), (2, 4), (3, 5)))
  }

  test("parejasCandidatas de un índice vacío no produce parejas") {
    assert(parejasCandidatas(Map.empty).isEmpty)
  }

  test("parejasCandidatas descarta publicaciones con un solo documento") {
    assert(parejasCandidatas(Map(Vector("a") -> Set(4))).isEmpty)
  }

  test("parejasCandidatas genera todas las combinaciones de una publicación") {
    assert(parejasCandidatas(Map(Vector("a") -> Set(3, 0, 2))) ==
      Vector((0, 2), (0, 3), (2, 3)))
  }

  test("parejasCandidatas ordena el resultado y elimina parejas repetidas") {
    val indice: Indice = Map(
      Vector("a") -> Set(5, 1, 3),
      Vector("b") -> Set(3, 1),
      Vector("c") -> Set(5, 1)
    )
    assert(parejasCandidatas(indice) ==
      Vector((1, 3), (1, 5), (3, 5)))
  }

  test("ranking ordena las mediciones de mayor a menor similitud") {
    val mediciones = Vector((0, 1, 0.4), (0, 2, 0.1), (1, 2, 0.9))
    assert(ranking(mediciones, 0.0, 10).map(_._3) == Vector(0.9, 0.4, 0.1))
  }

  test("ranking incluye el umbral y excluye las mediciones inferiores") {
    val mediciones = Vector((0, 1, 0.5), (0, 2, 0.49), (1, 2, 0.8))
    assert(ranking(mediciones, 0.5, 10) == Vector((1, 2, 0.8), (0, 1, 0.5)))
  }

  test("ranking rompe empates por la posición de las parejas") {
    val mediciones = Vector((2, 3, 0.7), (0, 2, 0.7), (0, 1, 0.7))
    assert(ranking(mediciones, 0.0, 10) ==
      Vector((0, 1, 0.7), (0, 2, 0.7), (2, 3, 0.7)))
  }

  test("ranking trunca el resultado al tope solicitado") {
    val mediciones = Vector((0, 1, 0.4), (0, 2, 0.1), (1, 2, 0.9))
    assert(ranking(mediciones, 0.0, 2) == Vector((1, 2, 0.9), (0, 1, 0.4)))
  }

  test("ranking vacío cuando el tope no es positivo") {
    val mediciones = Vector((0, 1, 0.4))
    assert(ranking(mediciones, 0.0, 0).isEmpty)
    assert(ranking(mediciones, 0.0, -1).isEmpty)
  }
}
