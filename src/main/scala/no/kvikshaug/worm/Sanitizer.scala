package no.kvikshaug.worm

object Sanitizer {
  def sanitize(str: String) = Converter.db match {
    case "sqlite" => str.replaceAll("'", "''")
    case _        => throw new UnsupportedDatabaseException("Worm doesn't support the '"+Converter.db+
                       "' DB engine yet.")
  }
}
