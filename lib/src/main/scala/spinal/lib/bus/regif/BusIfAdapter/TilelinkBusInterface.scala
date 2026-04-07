package spinal.lib.bus.regif

import spinal.core._
import spinal.lib.bus.tilelink
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.tilelink.Opcode

// ToDo: Force that the TL bus has no BCE?
// ToDo: Force that the TL bus has no burst support?
// ToDo: Use RegIf *_err for bus.d.corrupt and/or bus.d.denied? See AxiLite4 implementation (uses this.bus_slverr)
case class TilelinkBusInterface(
  bus: tilelink.Bus,
  sizeMap: SizeMapping,
  regPre: String = "",
  withSecFireWall: Boolean = false
)(implicit moduleName: ClassName) extends BusIf {
  override def getModuleName = moduleName.name

  override val busDataWidth = bus.p.dataWidth
  override val busAddrWidth = bus.p.addressWidth

  override val askWrite = False
  override val askRead  = (bus.a.valid || (bus.d.valid && !bus.d.ready)) && Opcode.A.isGet(bus.a.opcode)
  override val doWrite  = False
  override val doRead   = (bus.a.valid && (!bus.d.valid || bus.d.ready)) && Opcode.A.isGet(bus.a.opcode)

  // ToDo: What is the purpose of bus_rdata? We already have reg_rdata?
  override val reg_wrerr: Bool = Reg(Bool()) init(False)
  override val reg_rderr: Bool = Reg(Bool()) init(False)
  override val reg_rdata: Bits = Reg(Bits(busDataWidth bits)) init(defaultReadBits)
  override val bus_rdata: Bits = Bits(busDataWidth bits)
  override val writeData: Bits = bus.a.payload.data

  // ToDo: Can we support this? The AxiLite4 and Apb4 implementations do
  override val withStrb: Boolean = false
  override val wstrb: Bits = withStrb generate(Bits(strbWidth bit))
  override val wmask: Bits = withStrb generate(Bits(busDataWidth bit))
  override val wmaskn: Bits = withStrb generate(Bits(busDataWidth bit))
  initStrbMasks()

  val address = (bus.a.payload.address >> bus.p.dataBytesLog2Up) << bus.p.dataBytesLog2Up
  override def readAddress():  UInt = address
  override def writeAddress(): UInt = address

  // ToDo: Is this correct?
  /*
  val halt = False
  override def readHalt():  Unit = halt := True
  override def writeHalt(): Unit = halt := True
  */
  override def readHalt():  Unit = doRead := False
  override def writeHalt(): Unit = doWrite := False


  ////////


  bus.a.ready := bus.d.ready

  bus.d.valid := Reg(Bool()) init(False) clearWhen(bus.a.ready) setWhen(doRead)
  bus.d.data  := bus_rdata
  bus.d.opcode := Opcode.A.isGet(RegNextWhen(bus.a.opcode, bus.a.fire)).mux(Opcode.D.ACCESS_ACK_DATA(), Opcode.D.ACCESS_ACK())
  bus.d.param := 0
  bus.d.source := RegNextWhen(bus.a.payload.source, bus.a.fire)
  bus.d.size   := RegNextWhen(bus.a.payload.size,   bus.a.fire)
  bus.d.corrupt := False
  bus.d.denied  := False

  /*
  // Handle Tilelink D-Channel
  val respAsync = cloneOf(bus.d)
  respAsync.valid   := bus.a.valid && !halt
  respAsync.data    := bus_rdata
  respAsync.opcode  := Opcode.A.isGet(bus.a.opcode).mux(Opcode.D.ACCESS_ACK_DATA(), Opcode.D.ACCESS_ACK())
  respAsync.param   := 0
  respAsync.source  := bus.a.source
  respAsync.sink    := 0
  respAsync.size    := bus.a.size
  respAsync.corrupt := False
  respAsync.denied  := False
  bus.d << respAsync.stage()

  // Handle Tilelink A-Channel
  bus.a.ready := respAsync.ready && !halt
  */
}
