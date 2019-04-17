// See LICENSE.SiFive for license details.

package freechips.rocketchip.system

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.util.DontTouch

//ExampleRocketSystem是在RocketSubsystem的基础上进行扩展的。
//RocketSubsystem是在/subsystem/RocketSubsystem.scala中定义的类。
/** Example Top with periphery devices and ports, and a Rocket subsystem */
class ExampleRocketSystem(implicit p: Parameters) extends RocketSubsystem
    with HasAsyncExtInterrupts  //将Ext-Interrupts(PLIC的中断源)引入ibus中同步，即异步信号同步化。
                                //在/subsystem/InterruptBus.scala中定义。
    with CanHaveMasterAXI4MemPort
    with CanHaveMasterAXI4MMIOPort
    with CanHaveSlaveAXI4Port
    with HasPeripheryBootROM {  //调用BootROM模块。在/devices/tilelink/BootROM.scala中定义。
  override lazy val module = new ExampleRocketSystemModuleImp(this)

  // The sbus masters the cbus; here we convert TL-UH -> TL-UL
  sbus.crossToBus(cbus, NoCrossing)

  // The cbus masters the pbus; which might be clocked slower
  cbus.crossToBus(pbus, SynchronousCrossing())

  // The fbus masters the sbus; both are TL-UH or TL-C
  FlipRendering { implicit p =>
    sbus.crossFromBus(fbus, SynchronousCrossing())
  }

  // The sbus masters the mbus; here we convert TL-C -> TL-UH
  private val BankedL2Params(nBanks, coherenceManager) = p(BankedL2Key)
  private val (in, out, halt) = coherenceManager(this)
  if (nBanks != 0) {
    sbus.coupleTo("coherence_manager") { in :*= _ }
    mbus.coupleFrom("coherence_manager") { _ :=* BankBinder(mbus.blockBytes * (nBanks-1)) :*= out }
  }
}

//具体的端口连接模块
//RocketSubsystemModuleImp是在/subsystem/RocketSubsystem.scala中定义的类
class ExampleRocketSystemModuleImp[+L <: ExampleRocketSystem](_outer: L) extends RocketSubsystemModuleImp(_outer)
    with HasRTCModuleImp  //用于连接CLINT timer的时钟输入，可以直接将rtc_clk引到顶层中。在/subsystem/RTC.scala中定义。
    with HasExtInterruptsModuleImp  //将顶层的中断源(Ext-Interrupts)信号接到ExtInterruptsModule中。在/subsystem/InterruptBus.scala中定义。
    with CanHaveMasterAXI4MemPortModuleImp  //将AXI4协议的SRAM和DUT(ExampleRocketSystem)的memory_port总线(AXI4协议)相连。在/subsystem/Ports.scala中定义。
    with CanHaveMasterAXI4MMIOPortModuleImp //将AXI4协议的SRAM和DUT(ExampleRocketSystem)的mmio_port总线(AXI4协议)相连。在/subsystem/Ports.scala中定义。
    with CanHaveSlaveAXI4PortModuleImp  //将顶层的frontend_bus信号(AXI4协议)接到SlaveAXI4PortModule中。在/subsystem/Ports.scala中定义。
    with HasPeripheryBootROMModuleImp //将global_reset_vector接着BootROM的地址上，固定rocket-chip从BootROM开始启动。
                                      //在/devices/tilelink/BootROM.scala中定义。
    with DontTouch
