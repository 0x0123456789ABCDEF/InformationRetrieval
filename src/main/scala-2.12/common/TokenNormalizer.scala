package common

object TokenNormalizer extends (String => String) {
  override def apply(s: String): String = s.toLowerCase.trim
}
