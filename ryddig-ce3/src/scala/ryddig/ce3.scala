package ryddig

import cats.effect.{Resource, Sync}

object ce3 {
  def typed[F[_], U](l: TypedLogger[U])(implicit F: Sync[F]): TypedLoggerF[F, U] =
    new TypedLoggerF[F, U](l)

  def untyped[F[_]](l: Logger)(implicit F: Sync[F]): LoggerF[F] =
    l match {
      case l: TypedLogger[u] => new TypedLoggerF[F, u](l)
    }

  implicit class ResourceOps[A](private val res: LoggerResource[A]) extends AnyVal {
    def asResource[F[_]: Sync]: Resource[F, TypedLoggerF[F, A]] =
      Resource
        .make(Sync[F].delay(res.acquire())) { case LoggerResource.WithRelease(_, release) => Sync[F].delay(release()) }
        .map { case LoggerResource.WithRelease(logger, _) => new TypedLoggerF(logger) }
  }
}
