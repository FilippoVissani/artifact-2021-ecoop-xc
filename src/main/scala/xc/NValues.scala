package xc

import XCIncarnation._
import Builtins._

/**
 * This component provides a definition for `NValue`s.
 * This is a trait that requires to be mixed in with `XCLangImpl`.
 * In particular, this component:
 *
 * - Provides an `NValue` data type
 * - Provides built-ins operating on `NValue`s
 * - Provides implicit conversions to/from `NValue`s
 * - Provides utilities for working with `NValue`s of `Numeric`s
 */
trait NValues {
  self: XCLangImpl.XCLangSubComponent =>

  /**
   * Data type capturing neighbouring values, i.e., values modelling data received from or to be sent to neighbours.
   * This class defines a set of build-in methods for manipulating `NValue`s.
   *
   * @param m map from devices to corresponding values
   * @param default default value for neighbours for which no value is present in `m`
   * @tparam T type of neighbouring values
   */
  class NValue[T](val m: Map[ID, T], override val default: T) extends Defaultable[T] {
    implicit val defaultable: Defaultable[T] = this

    def defSubs(defaultValue: T): NValue[T] =
      NValue[T](this.m, defaultValue)

    def selfSubs(selfValue: T): NValue[T] =
      NValue[T](this.m ++ Map[ID, T](mid -> selfValue), this.default)

    def restricted: NValue[T] = { // TODO: contains on list {
      val nbrsSet = vm.alignedNeighbours().toSet
      NValue(this.m.filter(el => nbrsSet.contains(el._1)), this.default)
    }

    def flatMap[R](f: T => NValue[R]): NValue[R] =
      NValue(this.m.map { case (id, v) => id -> {
          val newEdgeField = f(v)
          newEdgeField.m.getOrElse(id, newEdgeField.default)
        }
      }, f(this.default))

    def mapWithId[R](o: (Option[ID], T) => R): NValue[R] =
      NValue(this.m.map(tp => tp._1 -> o(Some(tp._1), tp._2)), o(None, default))

    def map[R](o: T => R): NValue[R] =
      NValue(this.m.map(tp => tp._1 -> o(tp._2)), o(default))

    def map[R](defaultr: R, o: T => R): NValue[R] =
      NValue(this.m.map(tp => tp._1 -> o(tp._2)), defaultr)

    def map2[R, S](f: NValue[R])(o: (T, R) => S): NValue[S] =
      NValue((this.m.keySet ++ f.m.keySet).map(i => i -> o(this.m.getOrElse(i, this.default), f.m.getOrElse(i, f.default))).toMap,
        o(default, f.default))

    def map2l[R, S](f: NValue[R])(o: (T, R) => S): NValue[S] =
      NValue(this.m.map { case (i, v) => i -> o(v, f.m.getOrElse(i, f.default)) }, o(default, f.default))

    def map2r[R, S](f: NValue[R])(o: (T, R) => S): NValue[S] =
      NValue(f.m.map { case (i, v) => i -> o(this.m.getOrElse(i, this.default), v) }, o(default, f.default))

    def map2i[R, S](f: NValue[R])(o: (T, R) => S): NValue[S] =
      map2u(f)(this.default, f.default)(o)

    def map2d[R, S](f: NValue[R])(defaultr: R)(o: (T, R) => S): NValue[S] =
      NValue(this.m.map { case (i, v) => i -> o(v, f.m.getOrElse(i, defaultr)) }, o(default, defaultr))

    def map2u[R, S](f: NValue[R])(dl: T, dr: R)(o: (T, R) => S): NValue[S] =
      NValue((this.m.keys ++ f.m.keys).map { k => k -> o(this.m.getOrElse(k, dl), f.m.getOrElse(k, dr)) }.toMap,
        o(dl, dr))

    def fold[V >: T](z: V)(o: (V, V) => V): V =
      restricted.m.values.fold(z)(o)

    def reduce[V >: T](o: (V, V) => V): V =
      restricted.m.values.reduce(o)

    def minHood[V >: T](implicit ev: Bounded[V]): V =
      fold[V](ev.top) { case (a, b) => ev.min(a, b) }

    def minHoodPlus[V >: T](implicit ev: Bounded[V]): V =
      withoutSelf.minHood(ev)

    def withoutSelf: NValue[T] = NValue[T](this.m - mid, this.default)

    def toLocal: T = NValue.fieldToLocal(this)

    def toMap: Map[ID, T] = this.m

    override def toString: String = s"EdgeField[default=$default, exceptions=$m]"
  }

  /**
   * Companion object for `NValue`s: provides factory methods and implicit conversions.
   */
  object NValue {
    def apply[T](m: Map[ID, T])(implicit defaultable: Builtins.Defaultable[T]): NValue[T] =
      apply(m, defaultable.default)

    def apply[T](m: Map[ID, T], defaultValue: T): NValue[T] =
      new NValue(m, defaultValue)

    implicit def localToField[T](lv: T): NValue[T] =
      NValue(Map.empty, lv)

    implicit def fieldToLocal[T](fv: NValue[T]): T = {
      fv.m.getOrElse(mid, fv.default)
    }
  }

  /**
   * Syntactic sugar for numeric `NValue`s.
   */
  implicit class NumericNValue[T: Numeric](f: NValue[T]) extends Defaultable[T] {
    private val ev = implicitly[Numeric[T]]

    override def default: T = f.default

    implicit val defaultable: Defaultable[T] = this
    implicit val defaultableDouble: Defaultable[Double] = new Defaultable[Double] {
      override def default: Double = 0.0
    }

    def +(f2: NValue[T]): NValue[T] = f.map2i(f2)(ev.plus(_, _))

    def -(f2: NValue[T]): NValue[T] = f.map2i(f2)(ev.minus(_, _))

    def *(f2: NValue[T]): NValue[T] = f.map2i(f2)(ev.times(_, _))

    def +/[U](lv: U)(implicit uev: Numeric[U]): NValue[Double] = f.map[Double](ev.toDouble(_: T) + uev.toDouble(lv))

    def foldSum(init: T): NValue[T] = f.fold(init)(ev.plus)

    def foldSum(): NValue[T] = foldSum(ev.zero)
  }

  /**
   * Syntactic sugar for `NValue`s of pairs.
   */
  implicit class NValueOfTuples[A, B](f: NValue[Tuple2[A, B]]) {
    def _1: NValue[A] = f.map(_._1)

    def _2: NValue[B] = f.map(_._2)
  }

}
