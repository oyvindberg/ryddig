package ryddig

import org.slf4j
import org.slf4j.helpers.{BasicMDCAdapter, BasicMarkerFactory}
import org.slf4j.spi.{MDCAdapter, SLF4JServiceProvider}
import org.slf4j.{ILoggerFactory, IMarkerFactory, Marker}
import sourcecode.{File, Line}

import scala.math.Ordering.Implicits.infixOrderingOps

class RyddigSlf4jBridge extends SLF4JServiceProvider {
  override val getLoggerFactory: ILoggerFactory = name =>
    RyddigSlf4jBridge.globalLogger match {
      case Some(globalLogger) =>
        new RyddigSlf4jBridge.Instance(name, globalLogger)
      case None =>
        sys.error("You need to configure the logger in RyddigSlf4jBridge.globalLogger")
    }

  override def getMarkerFactory: IMarkerFactory = new BasicMarkerFactory

  override def getMDCAdapter: MDCAdapter = new BasicMDCAdapter

  override def getRequestedApiVersion: String = "2.0"

  override def initialize(): Unit = ()
}

object RyddigSlf4jBridge {
  var globalLogger: Option[Logger] = None

  class Instance(name: String, logger: Logger) extends slf4j.Logger {
    implicit val line: Line = Line(-1)
    implicit val file: File = File(name)

    def withMarker(logger: Logger)(m: Marker): Logger =
      logger.withContext("marker", m.toString)

    override def getName: String = name

    override def isTraceEnabled: Boolean = logger.minLogLevel <= LogLevel.debug

    override def trace(msg: String): Unit = logger.debug(msg)

    override def trace(format: String, arg: Object): Unit = logger.debug(format.formatted(arg))

    override def trace(format: String, arg1: Object, arg2: Object): Unit = logger.debug(format.formatted(arg1, arg2))

    override def trace(format: String, arguments: Object*): Unit = logger.debug(format.formatted(arguments*))

    override def trace(msg: String, t: Throwable): Unit = logger.debug(msg, t)

    override def isTraceEnabled(marker: Marker): Boolean = logger.minLogLevel <= LogLevel.debug

    override def trace(marker: Marker, msg: String): Unit = withMarker(logger)(marker).debug(msg)

    override def trace(marker: Marker, format: String, arg: Object): Unit = withMarker(logger)(marker).debug(format.formatted(arg))

    override def trace(marker: Marker, format: String, arg1: Object, arg2: Object): Unit =
      withMarker(logger)(marker).debug(format.formatted(arg1, arg2))

    override def trace(marker: Marker, format: String, argArray: Object*): Unit =
      withMarker(logger)(marker).debug(format.formatted(argArray*))

    override def trace(marker: Marker, msg: String, t: Throwable): Unit = withMarker(logger)(marker).debug(msg, t)

    override def isDebugEnabled: Boolean = logger.minLogLevel <= LogLevel.debug

    override def debug(msg: String): Unit = logger.debug(msg)

    override def debug(format: String, arg: Object): Unit = logger.debug(format.formatted(arg))

    override def debug(format: String, arg1: Object, arg2: Object): Unit = logger.debug(format.formatted(arg1, arg2))

    override def debug(format: String, arguments: Object*): Unit = logger.debug(format.formatted(arguments*))

    override def debug(msg: String, t: Throwable): Unit = logger.debug(msg, t)

    override def isDebugEnabled(marker: Marker): Boolean = logger.minLogLevel <= LogLevel.debug

    override def debug(marker: Marker, msg: String): Unit = withMarker(logger)(marker).debug(msg)

    override def debug(marker: Marker, format: String, arg: Object): Unit = withMarker(logger)(marker).debug(format.formatted(arg))

    override def debug(marker: Marker, format: String, arg1: Object, arg2: Object): Unit =
      withMarker(logger)(marker).debug(format.formatted(arg1, arg2))

    override def debug(marker: Marker, format: String, arguments: Object*): Unit =
      withMarker(logger)(marker).debug(format.formatted(arguments*))

    override def debug(marker: Marker, msg: String, t: Throwable): Unit = withMarker(logger)(marker).debug(msg, t)

    override def isInfoEnabled: Boolean = logger.minLogLevel <= LogLevel.info

    override def info(msg: String): Unit = logger.info(msg)

    override def info(format: String, arg: Object): Unit = logger.info(format.formatted(arg))

    override def info(format: String, arg1: Object, arg2: Object): Unit = logger.info(format.formatted(arg1, arg2))

    override def info(format: String, arguments: Object*): Unit = logger.info(format.formatted(arguments*))

    override def info(msg: String, t: Throwable): Unit = logger.info(msg, t)

    override def isInfoEnabled(marker: Marker): Boolean = logger.minLogLevel <= LogLevel.info

    override def info(marker: Marker, msg: String): Unit = withMarker(logger)(marker).info(msg)

    override def info(marker: Marker, format: String, arg: Object): Unit = withMarker(logger)(marker).info(format)

    override def info(marker: Marker, format: String, arg1: Object, arg2: Object): Unit =
      withMarker(logger)(marker).info(format.formatted(arg1, arg2))

    override def info(marker: Marker, format: String, arguments: Object*): Unit =
      withMarker(logger)(marker).info(format.formatted(arguments*))

    override def info(marker: Marker, msg: String, t: Throwable): Unit = withMarker(logger)(marker).info(msg, t)

    override def isWarnEnabled: Boolean = logger.minLogLevel <= LogLevel.warn

    override def warn(msg: String): Unit = logger.warn(msg)

    override def warn(format: String, arg: Object): Unit = logger.warn(format.formatted(arg))

    override def warn(format: String, arguments: Object*): Unit = logger.warn(format.formatted(arguments*))

    override def warn(format: String, arg1: Object, arg2: Object): Unit = logger.warn(format.formatted(arg1, arg2))

    override def warn(msg: String, t: Throwable): Unit = logger.warn(msg, t)

    override def isWarnEnabled(marker: Marker): Boolean = logger.minLogLevel <= LogLevel.warn

    override def warn(marker: Marker, msg: String): Unit = withMarker(logger)(marker).warn(msg)

    override def warn(marker: Marker, format: String, arg: Object): Unit = withMarker(logger)(marker).warn(format.formatted(arg))

    override def warn(marker: Marker, format: String, arg1: Object, arg2: Object): Unit =
      withMarker(logger)(marker).warn(format.formatted(arg1, arg2))

    override def warn(marker: Marker, format: String, arguments: Object*): Unit =
      withMarker(logger)(marker).warn(format.formatted(arguments*))

    override def warn(marker: Marker, msg: String, t: Throwable): Unit = withMarker(logger)(marker).warn(msg, t)

    override def isErrorEnabled: Boolean = logger.minLogLevel <= LogLevel.error

    override def error(msg: String): Unit = logger.error(msg)

    override def error(format: String, arg: Object): Unit = logger.error(format.formatted(arg))

    override def error(format: String, arg1: Object, arg2: Object): Unit = logger.error(format.formatted(arg1, arg2))

    override def error(format: String, arguments: Object*): Unit = logger.error(format.formatted(arguments*))

    override def error(msg: String, t: Throwable): Unit = logger.error(msg, t)

    override def isErrorEnabled(marker: Marker): Boolean = logger.minLogLevel <= LogLevel.error

    override def error(marker: Marker, msg: String): Unit = withMarker(logger)(marker).error(msg)

    override def error(marker: Marker, format: String, arg: Object): Unit = withMarker(logger)(marker).error(format.formatted(arg))

    override def error(marker: Marker, format: String, arg1: Object, arg2: Object): Unit =
      withMarker(logger)(marker).error(format.formatted(arg1, arg2))

    override def error(marker: Marker, format: String, arguments: Object*): Unit =
      withMarker(logger)(marker).error(format.formatted(arguments*))

    override def error(marker: Marker, msg: String, t: Throwable): Unit = withMarker(logger)(marker).error(msg, t)
  }
}
