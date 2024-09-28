package ryddig

import sourcecode.{Enclosing, File, Line}

import java.time.Instant
import scala.sys.process.ProcessLogger

@FunctionalInterface
trait LoggerFn { self =>
  def log[T: Formatter](value: => T, throwable: Option[Throwable], metadata: Metadata): Unit

  final def apply[T: Formatter](
      logLevel: LogLevel,
      t: => T,
      throwable: Option[Throwable] = None,
      instant: Instant = Instant.now
  )(implicit l: Line, f: File, e: Enclosing): Unit =
    this.log(t, throwable, new Metadata(instant, logLevel, l, f, e))

  @inline final def debug[T: Formatter](t: => T)(implicit l: Line, f: File, e: Enclosing): Unit =
    apply(LogLevel.debug, t)

  @inline final def debug[T: Formatter](t: => T, th: Throwable)(implicit l: Line, f: File, e: Enclosing): Unit =
    apply(LogLevel.debug, t, Some(th))

  @inline final def info[T: Formatter](t: => T)(implicit l: Line, f: File, e: Enclosing): Unit =
    apply(LogLevel.info, t)

  @inline final def info[T: Formatter](t: => T, th: Throwable)(implicit l: Line, f: File, e: Enclosing): Unit =
    apply(LogLevel.info, t, Some(th))

  @inline final def warn[T: Formatter](t: => T)(implicit l: Line, f: File, e: Enclosing): Unit =
    apply(LogLevel.warn, t)

  @inline final def warn[T: Formatter](t: => T, th: Throwable)(implicit l: Line, f: File, e: Enclosing): Unit =
    apply(LogLevel.warn, t, Some(th))

  @inline final def error[T: Formatter](t: => T)(implicit l: Line, f: File, e: Enclosing): Unit =
    apply(LogLevel.error, t)

  @inline final def error[T: Formatter](t: => T, th: Throwable)(implicit l: Line, f: File, e: Enclosing): Unit =
    apply(LogLevel.error, t, Some(th))

  final def and(other: LoggerFn): LoggerFn =
    new LoggerFn {
      override def log[T: Formatter](text: => T, throwable: Option[Throwable], metadata: Metadata): Unit = {
        this.log(text, throwable, metadata)
        other.log(text, throwable, metadata)
      }
    }

  final def processLogger(prefix: String)(implicit l: Line, f: File, e: Enclosing): ProcessLogger = {
    val separatedPrefix = if (prefix.isEmpty) prefix else s"$prefix: "
    ProcessLogger(
      out => info(separatedPrefix + out)(using implicitly, l, f, e),
      err => error(separatedPrefix + err)(using implicitly, l, f, e)
    )
  }
}
