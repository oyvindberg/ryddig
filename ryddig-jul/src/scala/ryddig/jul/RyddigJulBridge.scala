// Based on https://github.com/qos-ch/slf4j/blob/master/jul-to-slf4j/src/main/java/org/slf4j/bridge/SLF4JBridgeHandler.java
package ryddig.jul

import ryddig.{Logger, Metadata}
import sourcecode.{Enclosing, File, Line}

import java.text.MessageFormat
import java.time.Instant
import java.util.logging as jul
import scala.math.Ordering.Implicits.infixOrderingOps
import scala.util.Try

object RyddigJulBridge {
  @throws[SecurityException]
  def install(logger: Logger): Unit =
    jul.LogManager.getLogManager.getLogger("").addHandler(new RyddigJulBridge(logger))

  private def getRootLogger =
    jul.LogManager.getLogManager.getLogger("")

  @throws[SecurityException]
  def uninstall(): Unit =
    getRootLogger.getHandlers.foreach {
      case handler: RyddigJulBridge => getRootLogger.removeHandler(handler)
      case _                        => ()
    }

  def isInstalled: Boolean =
    getRootLogger.getHandlers.exists {
      case _: RyddigJulBridge => true
      case _                  => false
    }

  @throws[SecurityException]
  def removeHandlersForRootLogger(): Unit =
    getRootLogger.getHandlers.foreach(getRootLogger.removeHandler)

  def loggerNameFor(record: jul.LogRecord): String =
    Option(record.getLoggerName).getOrElse("<unknown java.util.logging logger>")

  def logLevelFor(level: jul.Level): ryddig.LogLevel =
    level match {
      case jul.Level.SEVERE  => ryddig.LogLevel.error
      case jul.Level.WARNING => ryddig.LogLevel.warn
      case jul.Level.INFO    => ryddig.LogLevel.info
      case jul.Level.CONFIG  => ryddig.LogLevel.info
      case _                 => ryddig.LogLevel.debug
    }

  /** Get the record's message, possibly via a resource bundle.
    */
  private def getMessageI18N(record: jul.LogRecord): Option[String] =
    Option(record.getMessage)
      .map { message =>
        Option(record.getResourceBundle).flatMap(bundle => Try(bundle.getString(message)).toOption).getOrElse(message)
      }
      .map { message =>
        Option(record.getParameters)
          .filter(_.nonEmpty)
          .flatMap(params => Try(MessageFormat.format(message, params)).toOption)
          .getOrElse(message)
      }

}

class RyddigJulBridge(logger: Logger) extends jul.Handler {
  override def close(): Unit = ()
  override def flush(): Unit = ()

  override def publish(record: jul.LogRecord): Unit =
    // Silently ignore null records.
    if (record == null) ()
    else {
      val logLevel = RyddigJulBridge.logLevelFor(record.getLevel)
      if (logger.minLogLevel <= logLevel) {
        val loggerName = RyddigJulBridge.loggerNameFor(record)
        val i18nMessage = RyddigJulBridge.getMessageI18N(record).getOrElse("<no message>")
        val metadata =
          new Metadata(Option(record.getInstant).getOrElse(Instant.now), logLevel, Line(-1), File(loggerName), Enclosing("bridge"))
        logger(i18nMessage, throwable = None, metadata)
      }
    }
}
