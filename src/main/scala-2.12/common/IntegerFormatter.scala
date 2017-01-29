package common

/**
  * Created by alosha on 1/29/17.
  */
object IntegerFormatter extends (Long => String) {
  override def apply(integer: Long): String = java.text.NumberFormat.getIntegerInstance.format(integer)
}
