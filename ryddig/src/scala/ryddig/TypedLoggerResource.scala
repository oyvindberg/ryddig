package ryddig

import java.io.Flushable

trait TypedLoggerResource[U] {
  def acquire(): (TypedLogger[U], () => Unit)

  final def use[T](f: TypedLogger[U] => T): T = {
    val (logger, release) = acquire()
    try f(logger)
    finally release()
  }
}

object TypedLoggerResource {
  final implicit class Ops[U1](private val one: TypedLoggerResource[U1]) extends AnyVal {
    def map[U2](transform: TypedLogger[U1] => TypedLogger[U2]): TypedLoggerResource[U2] =
      () => {
        val (logger, release) = one.acquire()
        (transform(logger), release)
      }

    def zipWith[U2](other: TypedLoggerResource[U2]): TypedLoggerResource[(U1, U2)] =
      () => {
        val (logger1, release1) = one.acquire()
        val (logger2, release2) = other.acquire()
        val releaseBoth: () => Unit = () => {
          release1()
          release2()
        }
        (logger1.zipWith(logger2), releaseBoth)
      }

    def maybeZipWith[U2](other: Option[TypedLoggerResource[U2]]): TypedLoggerResource[(U1, Option[U2])] =
      () => {
        val (logger1, release1) = one.acquire()
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

    def untyped: LoggerResource = () => {
      val (logger, release) = one.acquire()
      (logger.untyped, release)
    }
  }

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
