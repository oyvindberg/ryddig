package ryddig

import java.io.Flushable
import LoggerResource.WithRelease

@FunctionalInterface
trait LoggerResource[+U] {
  def acquire(): WithRelease[TypedLogger[U]]

  final def use[T](f: TypedLogger[U] => T): T = {
    val WithRelease(logger, release) = acquire()
    try f(logger)
    finally release()
  }

  final def map[U2](transform: TypedLogger[U] => TypedLogger[U2]): LoggerResource[U2] =
    () => {
      val WithRelease(logger, release) = acquire()
      WithRelease(transform(logger), release)
    }

  final def zipWith[U2](other: LoggerResource[U2]): LoggerResource[(U, U2)] =
    () => {
      val WithRelease(logger1, release1) = acquire()
      val WithRelease(logger2, release2) = other.acquire()
      val releaseBoth: () => Unit = () => {
        release1()
        release2()
      }
      WithRelease(logger1.zipWith(logger2), releaseBoth)
    }

  final def maybeZipWith[U2](other: Option[LoggerResource[U2]]): LoggerResource[(U, Option[U2])] =
    () => {
      val WithRelease(logger1, release1) = acquire()
      other match {
        case None =>
          WithRelease(logger1.maybeZipWith(None), release1)
        case Some(other) =>
          val WithRelease(logger2, release2) = other.acquire()
          val releaseBoth = () => {
            release1()
            release2()
          }
          WithRelease(logger1.maybeZipWith(Some(logger2)), releaseBoth)
      }
    }
}

object LoggerResource {
  case class WithRelease[+T](value: T, release: () => Unit)

  def pure[U](logger: TypedLogger[U]): LoggerResource[U] =
    () => WithRelease(logger, () => ())

  def autoCloseable[U <: AutoCloseable](mkLogger: => TypedLogger[U]): LoggerResource[U] =
    () => {
      val logger = mkLogger
      WithRelease(logger, () => logger.underlying.close())
    }

  def flushable[U <: Flushable](mkLogger: => TypedLogger[U]): LoggerResource[U] =
    () => {
      val logger = mkLogger
      WithRelease(logger, () => logger.underlying.flush())
    }
}
