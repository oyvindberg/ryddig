package ryddig

import sourcecode.{Enclosing, File, Line}

import java.time.Instant
import scala.Ordering.Implicits.*

trait LogActionsF[F[_]] extends Any {
  self: LoggerF[F] =>

  def unit: F[Unit]

  @inline final def log[T: Formatter](logLevel: LogLevel, t: => T)(implicit l: Line, f: File, e: Enclosing): F[Unit] =
    if (logLevel >= minLogLevel)
      self(t, None, new Metadata(Instant.now, logLevel, l, f, e))
    else unit

  @inline final def log[T: Formatter](logLevel: LogLevel, t: => T, th: Throwable)(implicit l: Line, f: File, e: Enclosing): F[Unit] =
    if (logLevel >= minLogLevel)
      self(t, Some(th), new Metadata(Instant.now, logLevel, l, f, e))
    else unit

  @inline final def debug[T: Formatter](t: => T)(implicit l: Line, f: File, e: Enclosing): F[Unit] =
    if ((LogLevel.debug: LogLevel) >= minLogLevel)
      self(t, None, new Metadata(Instant.now, LogLevel.debug, l, f, e))
    else unit

  @inline final def debug[T: Formatter](t: => T, th: Throwable)(implicit l: Line, f: File, e: Enclosing): F[Unit] =
    if ((LogLevel.debug: LogLevel) >= minLogLevel)
      self(t, Some(th), new Metadata(Instant.now, LogLevel.debug, l, f, e))
    else unit

  @inline final def info[T: Formatter](t: => T)(implicit l: Line, f: File, e: Enclosing): F[Unit] =
    if ((LogLevel.info: LogLevel) >= minLogLevel)
      self(t, None, new Metadata(Instant.now, LogLevel.info, l, f, e))
    else unit

  @inline final def info[T: Formatter](t: => T, th: Throwable)(implicit l: Line, f: File, e: Enclosing): F[Unit] =
    if ((LogLevel.info: LogLevel) >= minLogLevel)
      self(t, Some(th), new Metadata(Instant.now, LogLevel.info, l, f, e))
    else unit

  @inline final def warn[T: Formatter](t: => T)(implicit l: Line, f: File, e: Enclosing): F[Unit] =
    if ((LogLevel.warn: LogLevel) >= minLogLevel)
      self(t, None, new Metadata(Instant.now, LogLevel.warn, l, f, e))
    else unit

  @inline final def warn[T: Formatter](t: => T, th: Throwable)(implicit l: Line, f: File, e: Enclosing): F[Unit] =
    if ((LogLevel.warn: LogLevel) >= minLogLevel)
      self(t, Some(th), new Metadata(Instant.now, LogLevel.warn, l, f, e))
    else unit

  @inline final def error[T: Formatter](t: => T)(implicit l: Line, f: File, e: Enclosing): F[Unit] =
    if ((LogLevel.error: LogLevel) >= minLogLevel)
      self(t, None, new Metadata(Instant.now, LogLevel.error, l, f, e))
    else unit

  @inline final def error[T: Formatter](t: => T, th: Throwable)(implicit l: Line, f: File, e: Enclosing): F[Unit] =
    if ((LogLevel.error: LogLevel) >= minLogLevel)
      self(t, Some(th), new Metadata(Instant.now, LogLevel.error, l, f, e))
    else unit
}
