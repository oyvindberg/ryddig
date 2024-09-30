package ryddig

import sourcecode.{Enclosing, File, Line}

import scala.sys.process.ProcessLogger

object processLogger {
  def apply(logger: Logger, prefix: String)(implicit l: Line, f: File, e: Enclosing): ProcessLogger = {
    val separatedPrefix = if (prefix.isEmpty) prefix else s"$prefix: "
    ProcessLogger(
      out => logger.info(separatedPrefix + out)(using implicitly, l, f, e),
      err => logger.error(separatedPrefix + err)(using implicitly, l, f, e)
    )
  }
}
