package ryddig

import sourcecode.{Enclosing, File, Line}

import java.time.Instant

/** Prefer using [[Logger]] to this, as it produces more more efficient code.
  */
@FunctionalInterface
trait LoggerFn {
  def apply[T: Formatter](value: => T, throwable: Option[Throwable], metadata: Metadata): Unit
}

object LoggerFn {
  final implicit class LoggerFnOps(logger: LoggerFn) {
    def log[T: Formatter](logLevel: LogLevel, t: => T)(implicit l: Line, f: File, e: Enclosing): Unit =
      logger(t, None, new Metadata(Instant.now, logLevel, l, f, e))

    def log[T: Formatter](logLevel: LogLevel, t: => T, th: Throwable)(implicit l: Line, f: File, e: Enclosing): Unit =
      logger(t, Some(th), new Metadata(Instant.now, logLevel, l, f, e))

    def debug[T: Formatter](t: => T)(implicit l: Line, f: File, e: Enclosing): Unit =
      logger(t, None, new Metadata(Instant.now, LogLevel.debug, l, f, e))

    def debug[T: Formatter](t: => T, th: Throwable)(implicit l: Line, f: File, e: Enclosing): Unit =
      logger(t, Some(th), new Metadata(Instant.now, LogLevel.debug, l, f, e))

    def info[T: Formatter](t: => T)(implicit l: Line, f: File, e: Enclosing): Unit =
      logger(t, None, new Metadata(Instant.now, LogLevel.info, l, f, e))

    def info[T: Formatter](t: => T, th: Throwable)(implicit l: Line, f: File, e: Enclosing): Unit =
      logger(t, Some(th), new Metadata(Instant.now, LogLevel.info, l, f, e))

    def warn[T: Formatter](t: => T)(implicit l: Line, f: File, e: Enclosing): Unit =
      logger(t, None, new Metadata(Instant.now, LogLevel.warn, l, f, e))

    def warn[T: Formatter](t: => T, th: Throwable)(implicit l: Line, f: File, e: Enclosing): Unit =
      logger(t, Some(th), new Metadata(Instant.now, LogLevel.warn, l, f, e))

    def error[T: Formatter](t: => T)(implicit l: Line, f: File, e: Enclosing): Unit =
      logger(t, None, new Metadata(Instant.now, LogLevel.error, l, f, e))

    def error[T: Formatter](t: => T, th: Throwable)(implicit l: Line, f: File, e: Enclosing): Unit =
      logger(t, Some(th), new Metadata(Instant.now, LogLevel.error, l, f, e))
  }
}
