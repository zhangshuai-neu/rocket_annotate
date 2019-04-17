// See LICENSE.SiFive for license details.

package freechips.rocketchip.system

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.devices.debug.Debug
import freechips.rocketchip.diplomacy.LazyModule

class TestHarness()(implicit p: Parameters) extends Module {
  val io = new Bundle {
    val success = Bool(OUTPUT)
  }

  //注释：dut的复位是外部reset与dut debug模块的ndreset信号的或。
  val dut = Module(LazyModule(new ExampleRocketSystem).module)
  dut.reset := reset | dut.debug.ndreset

  dut.dontTouchPorts()
  dut.tieOffInterrupts()  //这个是将dut的中断信号全部接0
  dut.connectSimAXIMem()  //这些是原有的AXI接口，Mem和MMIO,在/subsystem/Ports.scala中定义。
  dut.connectSimAXIMMIO()
  dut.l2_frontend_bus_axi4.foreach(_.tieoff)  //这是frontend的接口，这个接口CPU是作为slave的，外部设备是作为master的,功能类似于SiFive的frontport，采用AXI4协议，
  Debug.connectDebug(dut.debug, clock, reset, io.success)   //注释：主要作用是将SimJTAG.v与dut相连，并输出io.success来确定仿真是否成功。
                                                            //注释：SimJTAG.v在/rocket-chip/vsim/generated-src/xxx目录中可以找到。
                                                            //注释：在/devices/debug/Periphery.scala中定义。
}
