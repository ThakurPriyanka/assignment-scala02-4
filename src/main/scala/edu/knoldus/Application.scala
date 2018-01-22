import org.apache.log4j.Logger

import scala.reflect.runtime.universe._
abstract class Commission(val commission: Int) {}

class ClientSideCommission(override val commission: Int) extends Commission(0) {}

class StreetSideCommission(override val commission: Int) extends Commission(0) {}

sealed trait CommissionDisplay {
  def totalDisplayCommission(typeCommission: String): String
}

class TotalCommission extends CommissionDisplay {
  override def totalDisplayCommission(typeCommission: String): String = {
    s"The total $typeCommission commission is"
  }

  def getTotalCommission[T <: Commission : TypeTag](commissionList: List[T]): String = {
    typeOf[T] match {
      case types if types =:= typeOf[Commission] => s"${totalDisplayCommission("Commission")} + ${commissionList.map(_.commission).sum}\n"
      case types if types <:< typeOf[ClientSideCommission] => s"${totalDisplayCommission("Client Commission")} + ${commissionList.map(_.commission).sum}\n"
      case types if types <:< typeOf[StreetSideCommission] => s"${totalDisplayCommission("Street Commission")} + ${commissionList.map(_.commission).sum}\n"
    }

  }
}

object Application {

  def main(args: Array[String]): Unit = {
    val log = Logger.getLogger(this.getClass)
    val num1 = 5
    val num2 = 10
    val num3 = 15
    val commission1:Commission = new ClientSideCommission(num1)
    val commission2:Commission = new StreetSideCommission(num2)
    val commission3:Commission = new ClientSideCommission(num3)
    val commissionList = List(commission1, commission2, commission3)
    val totalCommissionObj = new TotalCommission

    log.info(s"${totalCommissionObj.getTotalCommission[Commission](commissionList)}")

  }
}
