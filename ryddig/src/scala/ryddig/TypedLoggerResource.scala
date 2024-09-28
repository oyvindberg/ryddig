package ryddig

import java.io.Flushable

@FunctionalInterface
trait TypedLoggerResource[U] {
  def acquire(): (TypedLogger[U], () => Unit)

  final def use[T](f: TypedLogger[U] => T): T = {
    val (logger, release) = acquire()
    try f(logger)
    finally release()
  }

  final def map[U2](transform: TypedLogger[U] => TypedLogger[U2]): TypedLoggerResource[U2] =
    () => {
      val (logger, release) = acquire()
      (transform(logger), release)
    }

  final def zipWith[U2](other: TypedLoggerResource[U2]): TypedLoggerResource[(U, U2)] =
    () => {
      val (logger1, release1) = acquire()
      val (logger2, release2) = other.acquire()
      val releaseBoth: () => Unit = () => {
        release1()
        release2()
      }
      (logger1.zipWith(logger2), releaseBoth)
    }

  final def maybeZipWith[U2](other: Option[TypedLoggerResource[U2]]): TypedLoggerResource[(U, Option[U2])] =
    () => {
      val (logger1, release1) = acquire()
      other match {
        case None =>
          (logger1.maybeZipWith(None), release1)
        case Some(other) =>
          val (logger2, release2) = other.acquire()
          val releaseBoth = () => {
            release1()
            release2()
          }
          (logger1.maybeZipWith(Some(logger2)), releaseBoth)
      }
    }

  final def untyped: LoggerResource = () => {
    val (logger, release) = acquire()
    (logger.untyped, release)
  }
}

object TypedLoggerResource {
  def pure[U](logger: TypedLogger[U]): TypedLoggerResource[U] =
    () => (logger, () => ())

  def autoCloseable[U <: AutoCloseable](mkLogger: => TypedLogger[U]): TypedLoggerResource[U] =
    () => {
      val logger = mkLogger
      (logger, () => logger.underlying.close())
    }

  def flushable[U <: Flushable](mkLogger: => TypedLogger[U]): TypedLoggerResource[U] =
    () => {
      val logger = mkLogger
      (logger, () => logger.underlying.flush())
    }
}
