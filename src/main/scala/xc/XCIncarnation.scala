package xc

import it.unibo.scafi.incarnations.BasicAbstractIncarnation
import it.unibo.scafi.lib.StandardLibrary
import it.unibo.scafi.space.Point3D

object XCIncarnation extends BasicAbstractIncarnation with StandardLibrary {
  override implicit val idBounded: XCIncarnation.Builtins.Bounded[Int] = Builtins.Bounded.of_i
  override type Time = Double
  override type P = Point3D
}
