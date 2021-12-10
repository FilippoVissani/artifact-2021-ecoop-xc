package xc.examples

import xc.XCIncarnation._
import xc.{XCLangImpl, XCLib}

class Gradient extends AggregateProgram with StandardSensors with XCLangImpl with XCLib {
  override def main(): Any = {
    gradient(mid==2, senseDist)
  }
}

object GradientMain extends App {
  print("############ Hello XC ############\n")

  val program = new Gradient()

  // Now let's build a simplified system with sequential execution just to illustrate the execution model
  // Suppose the following topology: [1] -- [2] -- [3] -- [4] -- [5]
  // And that the source of the gradient is the device no. 2
  // Then, the expected result once the gradient has stabilised is: {1 -> 1, 2 -> 0, 3 -> 1, 4 -> 2, 5 -> 3}
  val devices = (1 to 5).toSeq
  val scheduling = devices ++ devices ++ devices ++ devices ++ devices // run 5 rounds each, in a round-robin fashion
  case class DeviceState(self: ID, exports: Map[ID,EXPORT], localSensors: Map[LSNS,Any], nbrSensors: Map[NSNS, Map[ID,Any]])
  val nbrRange: NSNS = NBR_RANGE
  var state: Map[ID,DeviceState] = (for(d <- devices; nbrs = Seq(d-1, d, d+1).filter(n => n > 0 && n < 6))
    yield d -> DeviceState(d, Map.empty[ID,EXPORT], Map.empty[LSNS,Any], Map[NSNS,Map[ID,Any]](nbrRange -> (nbrs.toSet[ID].map(nbr => nbr -> Math.abs(d - nbr).toDouble)).toMap))
  ).toMap
  // The following cycle performs the scheduling of rounds and simulates communication by writing on `state`
  for(d <- scheduling){
    val ctx = factory.context(selfId = d, exports = state(d).exports, lsens = state(d).localSensors, nbsens = state(d).nbrSensors)
    println(s"RUN: DEVICE ${d}\n\tCONTEXT: ${state(d)}")
    val export = program.round(ctx)
    state += d -> state(d).copy(exports = state(d).exports + (d -> export)) // update d's state
    // Simulate sending of messages to neighbours
    state(d).nbrSensors(nbrRange).keySet.foreach(nbr => {
      state += nbr -> state(nbr).copy(exports = state(nbr).exports + (d -> export))
    })
    println(s"\tEXPORT: ${export}\n\tOUTPUT: ${export.root()}\n--------------")
  }
}