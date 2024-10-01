package ryddig

import sourcecode.{Enclosing, File, Line}

import java.time.Instant
import scala.Ordering.Implicits.given

trait LogActionsF[F[_]] extends Any {
  self: LoggerF[F] =>

  def unit: F[Unit]

  private inline def doLog[T: Formatter](logLevel: LogLevel, t: => T, th: Option[Throwable], metadata: Metadata): F[Unit] =
    if logLevel >= minLogLevel then self(t, th, metadata) else unit

  final inline def log[T: Formatter](logLevel: LogLevel, t: => T)(using l: Line, f: File, e: Enclosing): F[Unit] =
    doLog(logLevel, t, None, new Metadata(Instant.now, logLevel, l, f, e))

  final inline def log[T: Formatter](logLevel: LogLevel, t: => T, th: Throwable)(using l: Line, f: File, e: Enclosing): F[Unit] =
    doLog(logLevel, t, Some(th), new Metadata(Instant.now, logLevel, l, f, e))

  final inline def debug[T: Formatter](t: => T)(using l: Line, f: File, e: Enclosing): F[Unit] =
    doLog(LogLevel.debug, t, None, new Metadata(Instant.now, LogLevel.debug, l, f, e))

  final inline def debug[T: Formatter](t: => T, th: Throwable)(using l: Line, f: File, e: Enclosing): F[Unit] =
    doLog(LogLevel.debug, t, Some(th), new Metadata(Instant.now, LogLevel.debug, l, f, e))

  final inline def info[T: Formatter](t: => T)(using l: Line, f: File, e: Enclosing): F[Unit] =
    doLog(LogLevel.info, t, None, new Metadata(Instant.now, LogLevel.info, l, f, e))

  final inline def info[T: Formatter](t: => T, th: Throwable)(using l: Line, f: File, e: Enclosing): F[Unit] =
    doLog(LogLevel.info, t, Some(th), new Metadata(Instant.now, LogLevel.info, l, f, e))

  final inline def warn[T: Formatter](t: => T)(using l: Line, f: File, e: Enclosing): F[Unit] =
    doLog(LogLevel.warn, t, None, new Metadata(Instant.now, LogLevel.warn, l, f, e))

  final inline def warn[T: Formatter](t: => T, th: Throwable)(using l: Line, f: File, e: Enclosing): F[Unit] =
    doLog(LogLevel.warn, t, Some(th), new Metadata(Instant.now, LogLevel.warn, l, f, e))

  final inline def error[T: Formatter](t: => T)(using l: Line, f: File, e: Enclosing): F[Unit] =
    doLog(LogLevel.error, t, None, new Metadata(Instant.now, LogLevel.error, l, f, e))

  final inline def error[T: Formatter](t: => T, th: Throwable)(using l: Line, f: File, e: Enclosing): F[Unit] =
    doLog(LogLevel.error, t, Some(th), new Metadata(Instant.now, LogLevel.error, l, f, e))
}
