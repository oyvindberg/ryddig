package ryddig

import cats.effect.{Resource, Sync}
import sourcecode.{Enclosing, File, Line}

import java.time.Instant

object ce3 {
  implicit class ResourceOps[A](private val res: TypedLoggerResource[A]) extends AnyVal {
    def asResource[F[_]: Sync]: Resource[F, TypedEffectfulLogger[F, A]] =
      Resource
        .make(Sync[F].delay(res.acquire())) { case (_, release) => Sync[F].delay(release()) }
        .map { case (logger, _) => new TypedEffectfulLogger(logger) }
  }
  type EffectfulLogger[F[_]] = TypedEffectfulLogger[F, Unit]

  class TypedEffectfulLogger[F[_]: Sync, A](val underlying: TypedLogger[A]) {
    def apply[T: Formatter](
        logLevel: LogLevel,
        t: => T,
        throwable: Option[Throwable] = None,
        instant: Instant = Instant.now
    )(implicit l: Line, f: File, e: Enclosing): F[Unit] =
      Sync[F].delay(underlying.apply(logLevel, t, throwable, instant)(using implicitly, l, f, e))

    @inline def debug[T: Formatter](t: => T)(implicit l: Line, f: File, e: Enclosing): F[Unit] =
      apply(LogLevel.debug, t)(using implicitly, l, f, e)

    @inline def debug[T: Formatter](t: => T, th: Throwable)(implicit l: Line, f: File, e: Enclosing): F[Unit] =
      apply(LogLevel.debug, t, Some(th))(using implicitly, l, f, e)

    @inline def info[T: Formatter](t: => T)(implicit l: Line, f: File, e: Enclosing): F[Unit] =
      apply(LogLevel.info, t)(using implicitly, l, f, e)

    @inline def info[T: Formatter](t: => T, th: Throwable)(implicit l: Line, f: File, e: Enclosing): F[Unit] =
      apply(LogLevel.info, t, Some(th))(using implicitly, l, f, e)

    @inline def warn[T: Formatter](t: => T)(implicit l: Line, f: File, e: Enclosing): F[Unit] =
      apply(LogLevel.warn, t)(using implicitly, l, f, e)

    @inline def warn[T: Formatter](t: => T, th: Throwable)(implicit l: Line, f: File, e: Enclosing): F[Unit] =
      apply(LogLevel.warn, t, Some(th))(using implicitly, l, f, e)

    @inline def error[T: Formatter](t: => T)(implicit l: Line, f: File, e: Enclosing): F[Unit] =
      apply(LogLevel.error, t)(using implicitly, l, f, e)

    @inline def error[T: Formatter](t: => T, th: Throwable)(implicit l: Line, f: File, e: Enclosing): F[Unit] =
      apply(LogLevel.error, t, Some(th))(using implicitly, l, f, e)

    def and[B](other: TypedEffectfulLogger[F, B]): TypedEffectfulLogger[F, (A, B)] =
      new TypedEffectfulLogger(underlying.zipWith(other.underlying))

    def untyped: EffectfulLogger[F] =
      new TypedEffectfulLogger(underlying.untyped)

    def minLogLevel(minLogLevel: LogLevel): TypedEffectfulLogger[F, A] =
      new TypedEffectfulLogger(underlying.withMinLogLevel(minLogLevel))

    def syncAccess: TypedEffectfulLogger[F, A] =
      new TypedEffectfulLogger(underlying.syncAccess)
  }
}
