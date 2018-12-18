package spinal.sim

import java.util.concurrent.locks.LockSupport

import net.openhft.affinity.Affinity

import scala.collection.mutable


trait SimThreadBlocker{
  def check() : Boolean
}

trait SimManagerSensitive{
  def update() : Boolean
}

class SimCallSchedule(val time: Long, val call : ()  => Unit){
  var next : SimCallSchedule = null
}

class JvmThreadUnschedule extends Exception

//Reusable thread
abstract class JvmThread(mainThread : Thread, creationThread : Thread, cpuAffinity : Int) extends Thread{
  var body : () => Unit = null
  private var unscheduleAsked = false
  def park() : Unit = {
    LockSupport.park()
    if(unscheduleAsked)
      throw new JvmThreadUnschedule()
  }

  def unschedule(): Unit ={
    unscheduleAsked = true
    LockSupport.unpark(this)
  }


  override def run(): Unit = {
    Affinity.setAffinity(cpuAffinity)
    LockSupport.unpark(creationThread)
    try {
      while (true) {
        park()
        body()
        body = null
        bodyDone()
        LockSupport.unpark(mainThread)
      }
    } catch{
      case _ : JvmThreadUnschedule =>
    }
  }

  def bodyDone() : Unit
}

class SimSuccess extends Exception
class SimFailure(message : String) extends Exception (message)

object SimManager{
  var cpuAffinity = 0
  var cpuCount = 0
  def newCpuAffinity() : Int = synchronized {
    if(cpuCount == 0) cpuCount = Runtime.getRuntime.availableProcessors
    val ret = cpuAffinity
    cpuAffinity = (cpuAffinity + 1) % cpuCount
    ret
  }
}

class SimManager(val raw : SimRaw) {
  val cpuAffinity = SimManager.newCpuAffinity()
  Affinity.setAffinity(cpuAffinity) //Boost context switching by 2 on host OS, by 10 on VM
  var threads : SimCallSchedule = null
  val mainThread = Thread.currentThread()

  val sensitivities = mutable.ArrayBuffer[SimManagerSensitive]()
  var commandBuffer = mutable.ArrayBuffer[() => Unit]()
  val onEndListeners = mutable.ArrayBuffer[() => Unit]()
  var time = 0l
  private var retains = 0
  var userData : Any = null
  val context = new SimManagerContext()
  context.manager = this
  SimManagerContext.threadLocal.set(context)

  //Manage the JvmThread poll
  val jvmBusyThreads = mutable.ArrayBuffer[JvmThread]()
  val jvmIdleThreads = mutable.Stack[JvmThread]()
  def newJvmThread(body : => Unit) : JvmThread = {
    if(jvmIdleThreads.isEmpty){
      val newJvmThread = new JvmThread(mainThread, Thread.currentThread(), cpuAffinity){
        override def bodyDone(): Unit = {
          jvmBusyThreads.remove(jvmBusyThreads.indexOf(this))
          jvmIdleThreads.push(this)
        }
      }
      jvmIdleThreads.push(newJvmThread)
      newJvmThread.start()
      LockSupport.park()
    }

    val jvmThread = jvmIdleThreads.pop()
    jvmThread.body = () => body

    jvmBusyThreads += jvmThread
    jvmThread
  }

  def setupJvmThread(thread: Thread){}
  def onEnd(callback : => Unit) : Unit = onEndListeners += (() => callback)
  def getInt(bt : Signal) : Int = raw.getInt(bt)
  def getLong(bt : Signal) : Long = raw.getLong(bt)
  def getBigInt(bt : Signal) : BigInt = raw.getBigInt(bt)
  def setLong(bt : Signal, value : Long): Unit = {
    bt.dataType.checkLongRange(value, bt)
    commandBuffer. += (() => raw.setLong(bt, value))
    assert(!commandBuffer.contains(null))
  }
  def setBigInt(bt : Signal, value : BigInt): Unit = {
    bt.dataType.checkBigIntRange(value, bt)
    commandBuffer += (() => raw.setBigInt(bt, value))
    assert(!commandBuffer.contains(null))
  }

  def schedule(thread : SimCallSchedule): Unit = {
    assert(thread.time >= time)
    var ptr = threads
    ptr match {
      case null => threads = thread
      case _ if ptr.time > thread.time => thread.next = threads; threads = thread
      case _ => {
        while(ptr.next != null && ptr.next.time <= thread.time){
          ptr = ptr.next
        }
        thread.next = ptr.next
        ptr.next = thread
      }
    }
  }

  def schedule(delay : Long)(thread : => Unit): Unit = {
    schedule( new SimCallSchedule(time + delay, () => thread))
  }

  def schedule(delay : Long, thread : SimThread): Unit = {
    schedule( new SimCallSchedule(time + delay, thread.resume))
  }


  def retain() = retains += 1
  def release() = retains -= 1

  def newThread(body : => Unit): SimThread ={
    val thread = new SimThread(body)
    schedule(delay=0, thread)
    thread
  }



  def run(body : => Unit): Unit ={
    val startAt = System.nanoTime()
    def threadBody(body : => Unit) : Unit = {
      body
      throw new SimSuccess
    }
    val tRoot = new SimThread(threadBody(body))
    schedule(delay=0, tRoot)
    runWhile(true)
    val endAt = System.nanoTime()
    val duration = (endAt - startAt)*1e-9
    println(f"""[Done] Simulation done in ${duration*1e3}%1.3f ms""")
  }


  def runAll(body : => Unit): Unit ={
    val startAt = System.nanoTime()
    val tRoot = new SimThread(body)
    schedule(delay=0, tRoot)
    runWhile(true)
    val endAt = System.nanoTime()
    val duration = (endAt - startAt)*1e-9
    println(f"""[Done] Simulation done in ${duration*1e3}%1.3f ms""")
  }

  def runWhile(continueWhile : => Boolean = true): Unit ={
    try {
//      simContinue = true
      var forceDeltaCycle = false
      var evalNanoTime = 0l
      var evalNanoTimeRef = System.nanoTime()
      while (((continueWhile || retains != 0) && threads != null/* && simContinue*/) || forceDeltaCycle) {
        //Process sensitivities

//        evalNanoTime -= System.nanoTime()
        var sensitivitiesCount = sensitivities.length
        var sensitivitiesId = 0
        while (sensitivitiesId < sensitivitiesCount) {
          if (!sensitivities(sensitivitiesId).update()) {
            sensitivitiesCount -= 1
            sensitivities(sensitivitiesId) = sensitivities(sensitivitiesCount)
            sensitivities.remove(sensitivitiesCount)
          } else {
            sensitivitiesId += 1
          }
        }
//        evalNanoTime += System.nanoTime()

        //Sleep until the next activity
        val nextTime = if(forceDeltaCycle) time else threads.time
        val delta = nextTime - time
        time = nextTime
        if (delta != 0) {
          raw.sleep(delta)
        }

        //Execute pending threads
        var tPtr = threads
        while (tPtr != null && tPtr.time == nextTime) {
          tPtr = tPtr.next
        }
        var tPtr2 = threads
        threads = tPtr
        while (tPtr2 != tPtr) {
          tPtr2.call()
          tPtr2 = tPtr2.next
        }


        //Evaluate the hardware outputs
        if(forceDeltaCycle){
//          if(false) {
            raw.eval()
//          } else {
////            evalNanoTime -= System.nanoTime()
//            raw.eval()
//            val nano =  System.nanoTime()
////            evalNanoTime += nano
//            val elabsedTime = nano - evalNanoTimeRef
//            if(elabsedTime > 1000000000){
//              println(s"[Info] Simulator evaluation use ${100*evalNanoTime/elabsedTime}% of the processing")
//              evalNanoTimeRef = nano
//              evalNanoTime = 0
//            }
//          }
        }



        //Execute the threads commands
        forceDeltaCycle = !commandBuffer.isEmpty
        if(forceDeltaCycle){
          commandBuffer.foreach(_())
          commandBuffer.clear()
        }
      }
      if(retains != 0){
        throw new SimFailure("Simulation ended while there was still some retains")
      }
    } catch {
      case e : SimSuccess =>
      case e : Throwable => {
        println(f"""[Error] Simulation failed at time=$time""")
        throw e
      }
    } finally {
      (jvmIdleThreads ++ jvmBusyThreads).foreach(t => t.unschedule())
      for(t <- (jvmIdleThreads ++ jvmBusyThreads)){
        while(t.isAlive()){Thread.sleep(0)}
      }
      raw.sleep(1)
      raw.end()
      onEndListeners.foreach(_())
      SimManagerContext.threadLocal.set(null)
    }
  }
}
