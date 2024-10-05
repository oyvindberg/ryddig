package ryddig

import cats.effect.Sync

sealed abstract class LoggerF[F[_]: Sync](val raw: Logger) extends LogActionsF[F] {
  final val minLogLevel: LogLevel =
    raw.minLogLevel

  final def apply[T: Formatter](value: => T, throwable: Option[Throwable], metadata: Metadata): F[Unit] =
    Sync[F].delay(raw.apply(value, throwable, metadata))
}

object LoggerF {
  implicit class LoggerOps[F[_]](l: LoggerF[F]) {
    def withContext[T: Formatter](key: String, value: T): LoggerF[F] =
      l match {
        case l: TypedLoggerF[F, t] => l.withContext(key, value)
      }

    def withOptContext[T: Formatter](key: String, maybeValue: Option[T]): LoggerF[F] =
      l match {
        case l: TypedLoggerF[F, t] => l.withOptContext(key, maybeValue)
      }

    def withPath(fragment: String): LoggerF[F] =
      l match {
        case l: TypedLoggerF[F, t] => l.withPath(fragment)
      }

    def withMinLogLevel(minLogLevel: LogLevel): LoggerF[F] =
      l match {
        case l: TypedLoggerF[F, t] => l.withMinLogLevel(minLogLevel)
      }

    def zipWith(other: LoggerF[F]): LoggerF[F] =
      (l, other) match {
        case (l1: TypedLoggerF[F, t1], l2: TypedLoggerF[F, t2]) => l1.zipWith(l2)
      }

    def maybeZipWith(other: Option[LoggerF[F]]): LoggerF[F] =
      (l, other) match {
        case (l1: TypedLoggerF[F, t1], Some(l2: TypedLoggerF[F, t2])) => l1.maybeZipWith(Some(l2))
        case (l1: TypedLoggerF[F, t1], None)                          => l1.maybeZipWith(None)
      }

    def syncAccess(on: Object): LoggerF[F] =
      l match {
        case l: TypedLoggerF[F, t] => l.syncAccess(on)
      }
  }
}

final class TypedLoggerF[F[_]: Sync, +U](override val raw: TypedLogger[U]) extends LoggerF[F](raw) {
  val underlying: U = raw.underlying

  override def unit: F[Unit] = Sync[F].unit

  def withContext[T: Formatter](key: String, value: T): TypedLoggerF[F, U] =
    new TypedLoggerF(raw.withContext(key, value))

  def withPath(fragment: String): TypedLoggerF[F, U] =
    new TypedLoggerF(raw.withPath(fragment))

  def withOptContext[T: Formatter](key: String, maybeValue: Option[T]): TypedLoggerF[F, U] =
    maybeValue match {
      case Some(value) => withContext(key, value)
      case None        => this
    }

  def zipWith[U2](other: TypedLoggerF[F, U2]): TypedLoggerF[F, (U, U2)] =
    new TypedLoggerF(raw.zipWith(other.raw))

  def maybeZipWith[U2](other: Option[TypedLoggerF[F, U2]]): TypedLoggerF[F, (U, Option[U2])] =
    new TypedLoggerF(raw.maybeZipWith(other.map(_.raw)))

  def withMinLogLevel(level: LogLevel): TypedLoggerF[F, U] =
    new TypedLoggerF(raw.withMinLogLevel(level))

  def map[U2](f: U => U2): TypedLoggerF[F, U2] =
    new TypedLoggerF(raw.map(f))

  def syncAccess(on: Object): TypedLoggerF[F, U] =
    new TypedLoggerF(raw.syncAccess(on))
}
