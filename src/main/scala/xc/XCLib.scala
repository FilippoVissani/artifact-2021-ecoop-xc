package xc

import XCIncarnation._

trait XCLib extends StandardSensors {
  self: XCLangImpl.XCLangSubComponent =>

  /**
   * Expresses FC's `nbr` construct in terms of exchange`
   */
  def nbrByExchange[A](e: => NValue[A]): NValue[A] =
    exchange[(A, A)](e.map2(e)((_, _)))(n => n.map2(e) { case (n, e) => (e, n._1) }).map(_._2) // NB: exchange(..)(..)._2 would compile but doesn't work

  /**
   * Expresses FC's `nbr` construct in terms of `exchange` -- working on local values
   */
  def nbrLocalByExchange[A](e: => A): NValue[A] =
    exchange[(A, NValue[A])]((e, NValue.localToField(e)))(n => (e, n.map(_._1)))._2

  /**
   * Expresses FC's `rep` construct in terms of `exchange``
   */
  def repByExchange[A](init: => A)(f: (A) => A): A =
    exchangeFull(init)(p => f(p.old))

  /**
   * Expresses FC's `share` construct in terms of `exchange`
   */
  def shareByExchange[A](init: A)(f: A => A): A =
    exchange(init)(n => f(n))

  /**
   * @return a pair of `NValue`s
   */
  def pair[A, B](a: NValue[A], b: NValue[B]): NValue[(A, B)] =
    a.map2(b)((_, _))

  /**
   * Broadcasts value `value` along the gradient `dist`.
   * @param dist Potential field indicating the direction of information (and the point of the `value` to broadcast)
   * @param value The value to be broadcasted
   * @return The value broadcasted by the neighbour at minimum `dist`
   */
  def broadcast[T](dist: Double, value: T): T = {
    val loc = (dist, value)
    exchange[(Double, T)](loc)(n => {
      (dist, n.withoutSelf.fold[(Double, T)](loc)((t1, t2) => if (t1._1 <= t2._1) t1 else t2)._2)
    })._2
  }

  /**
   * Self-organising distance computation algorithm: it computes, in each node of the system, the distance
   * from the closest `source`, according to `metric`. This is a distributed varion of the Bellman-Ford algorithm
   */
  def distanceTo(source: Boolean, metric: NValue[Double]): Double =
    exchange(Double.PositiveInfinity)(n =>
      mux(source) {
        0.0
      } {
        (n + metric).withoutSelf.fold(Double.PositiveInfinity)(Math.min)
      }
    )

  /**
   * This block computes the distance between `source` and `dest` and propagates that information on the whole network.
   * @param source One endpoint of the distance calculation
   * @param dest The other endpoint of the distance calculation
   * @return the local view of the computed distance between `source` and `dest`
   */
  def distance(source: Boolean, dest: Boolean): Double = {
    broadcast(distanceTo(source, senseDist), distanceTo(dest, senseDist))
  }

  /**
   * Self-organising channel computation: it yields, in each node of the system, a Boolean indicating whether
   * that node belongs to the channel connecting `source` with `dest` or not.
   * @param source One endpoint of the channel
   * @param dest Another andpoint of the channel
   * @param width Parameter providing redundancy to the channel
   * @return a Boolean indicating whether the node belongs to the channel or not
   */
  def channel(source: Boolean, dest: Boolean, width: Double): Boolean = {
    val ds = distanceTo(source, senseDist)
    val dd = distanceTo(dest, senseDist)
    val dsd = distance(source, dest)
    if (ds == Double.PositiveInfinity || dd == Double.PositiveInfinity || dsd == Double.PositiveInfinity) false else ds + dd < dsd + width
  }

  /**
   * Function capturing a distributed collection process from all the devices in the system towards `sink`.
   * In particular, it implements a weighted multi-path collection.
   */
  def collect[T](sink: Boolean, radius: Double, value: T, Null: T,
                 accumulate: (NValue[T], T) => T,
                 extract: (NValue[T], NValue[Double], NValue[Double], NValue[T]) => NValue[T],
                 threshold: Double = 0.1
                ): T = {
    def weight(dist: Double, radius: Double): NValue[Double] = {
      val distDiff: NValue[Double] = nbrLocalByExchange(dist).map(v => Math.max(dist - v, 0))
      val res = NValue.localToField(radius).map2(senseDist)(_ - _).map2(distDiff)(_ * _)
      // NB: NaN values may arise when `dist`s are Double.PositiveInfinity (e.g., inf - inf = NaN)
      res.map(v => if (v.isNaN) 0 else v)
    }

    def normalize(w: NValue[Double]): NValue[Double] = {
      val sum: Double = w.foldSum
      val res = w.map(_ / sum)
      res.map(v => if (v.isNaN) 0 else v)
    }

    val dist = gradient(sink, senseDist)
    exchange(value)(n => {
      val loc: T = accumulate(n.withoutSelf, value) // or also: accumulate(selfSubs(n, 0.0),value)
      val w: NValue[Double] = weight(dist, radius)
      val normalized: NValue[Double] = normalize(w)
      val res: NValue[T] = extract(loc, normalized, threshold, Null)
      selfSubs(res, loc)
    })
  }

  /**
   * Self-organising distance computation algorithm: it computes, in each node of the system, the distance
   * from the closest `source`, according to `metric`. This is a distributed varion of the Bellman-Ford algorithm.
   */
  def gradient(source: Boolean, metric: NValue[Double]): Double = exchange(Double.PositiveInfinity)(n =>
    mux(source) {
      0.0
    } {
      (n + metric).withoutSelf.fold(Double.PositiveInfinity)(Math.min)
    }
  )

  /**
   * This implements a self-healing gradient based on the Bellman-Ford algorithm using hops as distance metrics.
   */
  def hopGradient(src: Boolean): NValue[Int] = exchange(Double.PositiveInfinity)(n =>
    mux(src) {
      0.0
    } {
      n.withoutSelf.fold(Double.PositiveInfinity)(Math.min) + 1
    }
  ).toInt

  /**
   * Builds an `NValue` from a field expressed by `e`
   */
  def fsns[A](e: => A, defaultA: A): NValue[A] =
    NValue[A](includingSelf.reifyField(e), defaultA)

  /**
   * The neighbouring sensor providing distances to neighbours, as a function providing an `NValue`
   */
  def senseDist: NValue[Double] = fsns(nbrRange, Double.PositiveInfinity)

  /**
   * Weights corresponding to neighbours are calculated in order to penalise devices that are likely to lose
   * their “receiving” status, a situation that can happen in two cases:
   * (1) if the “receiving” device is too close to the edge of proximity
   * of the “sending” device, it might step outside of it in the immediate future breaking the connection;
   * (2) if the potential of the “receiving” device is too close to the potential of the “sending” device,
   * their relative role of sender/receiver might be switched in the immediate future, possibly
   * creating an “information loop” between the two devices.
   * w is positive and symmetric.
   */
  def Cwmp(sink: Boolean, radius: Double, value: Double, Null: Double, threshold: Double = 0.1): Double = {
    def accumulate[T: Numeric](v: NValue[T], l: T): T = /* E.g., MAX: implicitly[Builtins.Bounded[T]].max(v,l) */
      v.foldSum(l)

    def extract(v: NValue[Double], w: NValue[Double], threshold: NValue[Double], Null: NValue[Double]): NValue[Double] = //if(w > threshold) v else Null
      v * w

    def weight(dist: Double, radius: Double): NValue[Double] = {
      val distDiff: NValue[Double] = nbrLocalByExchange(dist).map(v => Math.max(dist - v, 0))
      val res = NValue.localToField(radius).map2(senseDist)(_ - _).map2(distDiff)(_ * _)
      // NB: NaN values may arise when `dist`s are Double.PositiveInfinity (e.g., inf - inf = NaN)
      res.map(v => if (v.isNaN) 0 else v)
    }

    def normalize(w: NValue[Double]): NValue[Double] = {
      val sum: Double = w.foldSum
      val res = w.map(_ / sum)
      res.map(v => if (v.isNaN) 0 else v)
    }

    val dist = gradient(sink, senseDist)
    exchange(value)(n => {
      val loc: Double = accumulate(n.withoutSelf, value) // or also: accumulate(selfSubs(n, 0.0),value)
      val w: NValue[Double] = weight(dist, radius)
      val normalized: NValue[Double] = normalize(w)
      val res: NValue[Double] = extract(loc, normalized, threshold, Null)
      selfSubs(res, loc)
    })
  }

  /**
   * - Every node receives by a single parent
   * - Every node transmits to all those neighbours that chose it as a parent
   * - The parent is chosen as the node with minimum distance
   */
  def optimisedBroadcast[T](distance: Double, value: T, Null: T): T = {
    // (default is the self-key)
    val nbrKey: NValue[(Double, ID)] = nbrLocalByExchange((distance, mid))
    // `parent` is a Boolean field that holds true for the device that chose the current device as a parent
    //   (default is the true if the self-key is equal to the min-key---this is true only for the source)
    val parent = nbrKey.map(_ == nbrKey.fold[(Double, ID)](nbrKey)((t1, t2) => if (t1._1 < t2._1) t1 else t2))
    exchange(value)(n => {
      val _loc = n.map2(nbrLocalByExchange(distance)) { case (v, d) => (v == Null, d, v) }
        .fold((false, distance, value))((t1, t2) => if (!t1._1 && t2._1) t1 else if (t1._2 < t2._2) t1 else t2)
      val loc = _loc._3
      val nbrParent = nbrByExchange(parent)
      // (default value of `res` is `Null` for every device except sources)
      val res = selfSubs(nbrParent.map(mux(_) {
        loc
      } {
        Null
      }), loc) // defSubs(..., Null) ?
      // println(s"${mid} => nbrKey ${nbrKey} parent ${parent} N ${n} _loc ${_loc} nbrParent ${nbrParent} res ${res}")
      res
    })
  }

  def biConnection(): NValue[Int] = exchange(0)(n => n + defSubs(1, 0))

  /**
   * A collection function (not used in the case study).
   */
  def Csubj[P: Builtins.Bounded, V](sink: Boolean, value: V, acc: (V, V) => V, divide: (V, Double) => V): V = {
    val dist: Int = hopGradient(sink)
    // Use exchange to handle communication of distances (dist) and collected values
    exchange[(Int, V)]((dist, value))(n => {
      // The reliability of a downstream neighbor (with lower values of `dist` wrt self) is estimated by the
      //   the corresponding connection time.
      // val conn: EdgeField[Int] = mux(n.map(_._1).fold(Int.MaxValue)(Math.min) < dist){ biConnection() }{ 0 }
      val conn: NValue[Int] = pair(n, biConnection()).map { case (n, biconn) => mux(n._1 < dist) {
        biconn
      } {
        0
      }
      }
      // Reliability scores are normalised in `send`, obtaining percetanges
      val send: NValue[Double] = conn.map(_.toDouble / Math.max(1, conn.foldSum))
      // Let's collect the `send` scores into `recv` scores for receiving neighbours' messages
      val recv: NValue[Double] = nbrByExchange(send)
      // Now, values of neighbours (`n.map(_._2)`) are weighted with `recv` scores through given `divide` function
      val weightedValues: NValue[V] = pair(n.map(_._2), recv).map(v => divide(v._1, v._2))
      // Finally, use `acc` to aggregate neighbours' contributions
      val collectedValue: V = weightedValues.fold(value)(acc)
      // println(s"${mid} => n = $n dist = ${n._1} conn = $conn send = $send recv = $recv weightedvals = $weightedValues collectedValue = $collectedValue")
      pair(dist: NValue[Int], collectedValue)
    })._2
  }

  /**
   * Retains a value `v` for `retentionTime` rounds, before switching to the new `value`.
   */
  def keep[T](value: T, retentionTime: Long, iff: T => Boolean) = repByExchange((value, 0L))(v => {
    if (iff(v._1) && v._2 <= retentionTime) (v._1, v._2 + 1) else (value, 0L)
  })._1

  /**
   * Substitution of the default
   */
  def defSubs[T](ef: NValue[T], defaultValue: T): NValue[T] =
    NValue(vm.alignedNeighbours().map(id => id -> ef.m.getOrElse(id, ef.default)).toMap, defaultValue)

  /**
   * Substitution of the value for "self"
   */
  def selfSubs[T](ef: NValue[T], selfValue: T): NValue[T] =
    NValue(ef.m ++ Map[ID, T](mid -> selfValue), ef.default)
}
