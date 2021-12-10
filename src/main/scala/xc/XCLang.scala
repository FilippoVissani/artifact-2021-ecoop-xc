package xc

import XCIncarnation._

/**
 * Interface representing XC constructs
 */
trait XCLang {
  /**
   * Abstract type for a neighbouring value of `T`s
   */
  type NV[T]

  /**
   * This operator branches the computation into `th` or `el` according to `cond`.
   */
  def xcbranch[T](cond: NV[Boolean])(th: => NV[T])(el: => NV[T]): NV[T]

  /**
   * This single operator handles state and message reception/sending.
   * @param init initial value for new devices
   * @param f function from neighbouring value to neighbouring value
   * @tparam T the type of neighbouring values
   * @return the neighbouring value providing for the next local state and messages for neighbours
   */
  def exchange[T](init: NV[T])(f: NV[T] => NV[T] /*(NV[T], NV[T])*/): NV[T]
}

/**
 * This trait provides an implementation of the eXchange Calculus (XC).
 */
trait XCLangImpl extends XCLang with NValues with FieldUtils {
    self: FieldCalculusSyntax with ExecutionTemplate =>

  override type NV[T] = NValue[T]

  /**
   * @inheritdoc
   */
  override def xcbranch[T](cond: NValue[Boolean])(th: => NValue[T])(el: => NValue[T]): NValue[T] =
    branch(cond.toLocal)(th)(el)

  /**
   * @inheritdoc
   */
  override def exchange[A](init: NValue[A])(f: NValue[A] => NValue[A]): NValue[A] = {
    val rvm = vm.asInstanceOf[RoundVMImpl]
    vm.nest(Scope(s"$EXCHANGE_SLOT${vm.index}"))(write = true) {
      val nbrs = vm.alignedNeighbours

      def readNeighbourValue[A](id: ID): Option[A] = rvm.context.readSlot(id, rvm.status.path)

      val inputEdgeField = new NValue[A](
        nbrs.map(nbrId => { // create the nvalue by getting the contributions from all the neighbours (`nbrs`)
          nbrId -> readNeighbourValue[NValue[A]](nbrId) // get nvalue received from device `nbrId`
            .getOrElse(init).m // if there is not an aligned export from `nbrId`, use `init`
            .getOrElse(vm.self, // from the neighbour's nvalue, get the value sent to the current device (`self`)
              readNeighbourValue[NValue[A]](nbrId).getOrElse(init).default // otherwise, provide the default
            )
        }).toMap,
        init.default)
      f(inputEdgeField)
    }
  }

  /**
   * Another version of `exchange` with different signatures, accepting a function from "old"
   * and "neighbouring" `NValue`s to `NValue`.
   */
  def exchangeFull[A](init: NValue[A])(f: ExchangeParams[A] => NValue[A]): NValue[A] = {
    val rvm = vm.asInstanceOf[RoundVMImpl]
    vm.nest(Scope(s"$EXCHANGE_SLOT${vm.index}"))(write = true) {
      val nbrs = vm.alignedNeighbours

      def nbrEdgeValue[A](id: ID): Option[A] = rvm.context.readSlot(id, rvm.status.path)

      val oldEdgeField = rvm.context.readSlot(vm.self, rvm.status.path).getOrElse(init)
      val inputEdgeField = new NValue[A](
        nbrs.map(nbrId =>
          nbrId -> nbrEdgeValue[NValue[A]](nbrId)
            .getOrElse(init).m
            .getOrElse(vm.self, nbrEdgeValue[NValue[A]](nbrId).getOrElse(init).default)
        ).toMap,
        init.default)
      val outputEdgeField = f(ExchangeParams(oldEdgeField, inputEdgeField))
      outputEdgeField
    }
  }

  case class ExchangeParams[T](old: NValue[T], neigh: NValue[T])

  object ExchangeParams {
    implicit def fromTuple[T](tuple: (NValue[T], NValue[T])): ExchangeParams[T] =
      ExchangeParams(tuple._1, tuple._2)
  }

  /**
   * Slot object corresponding to a value tree node
   */
  private val EXCHANGE_SLOT = Scope("exchange")
}

/**
 * Companion object. It defines a type which is instrumental for splitting the content of this "family" trait.
 */
object XCLangImpl {
  type XCLangSubComponent = XCLangImpl with NValues with FieldUtils with FieldCalculusSyntax with ExecutionTemplate
}
