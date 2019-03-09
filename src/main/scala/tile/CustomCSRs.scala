// See LICENSE.SiFive for license details.

package freechips.rocketchip.tile

import chisel3._

import freechips.rocketchip.config.Parameters

//每个CustomCSR都有一个id
case class CustomCSR(id: Int, mask: BigInt, init: Option[BigInt])

object CustomCSR {
  def constant(id: Int, value: BigInt): CustomCSR = CustomCSR(id, BigInt(0), Some(value))
}

//CSR的输入和输出端口
class CustomCSRIO(implicit p: Parameters) extends CoreBundle {
  val wen = Bool()
  val wdata = UInt(xLen.W)
  val value = UInt(xLen.W)
}

class CustomCSRs(implicit p: Parameters) extends CoreBundle {
  // Not all cores have these CSRs, but those that do should follow the same
  // numbering conventions.  So we list them here but default them to None.

  //bpmCSR代表的是分支预测模式寄存器
  protected def bpmCSRId = 0x7c0
  protected def bpmCSR: Option[CustomCSR] = None

  protected def chickenCSRId = 0x7c1
  protected def chickenCSR: Option[CustomCSR] = None

  // If you override this, you'll want to concatenate super.decls
  def decls: Seq[CustomCSR] = bpmCSR.toSeq ++ chickenCSR

  //代表所有的CustomCSR的输入和输出引脚
  val csrs = Vec(decls.size, new CustomCSRIO)

  //判断bpmCSR和chickenCSR的对应位，其中bpmCSR和chickenCSR是由类CustomCSR实例化出来的
  //通过getOrElse来判断如下对应的功能是否开启
  def flushBTB = getOrElse(bpmCSR, _.wen, false.B)
  def bpmStatic = getOrElse(bpmCSR, _.value(0), false.B)
  def disableDCacheClockGate = getOrElse(chickenCSR, _.value(0), true.B)
  def disableICacheClockGate = getOrElse(chickenCSR, _.value(1), true.B)
  def disableCoreClockGate = getOrElse(chickenCSR, _.value(2), true.B)

  protected def getByIdOrElse[T](id: Int, f: CustomCSRIO => T, alt: T): T = {
    val idx = decls.indexWhere(_.id == id)  //判断decls中是否存在当前id
    if (idx < 0) alt else f(csrs(idx))      //如果不存在返回alt，反之则返回CustomCSRIO中wen或value的值
  }

  protected def getOrElse[T](csr: Option[CustomCSR], f: CustomCSRIO => T, alt: T): T =
    csr.map(c => getByIdOrElse(c.id, f, alt)).getOrElse(alt)  //最后的getOrElse不是当前类中定义的，而是Vec中的
}
