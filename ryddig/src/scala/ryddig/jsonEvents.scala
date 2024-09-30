package ryddig

import fansi.Str
import io.circe.generic.semiauto
import io.circe.parser.decode
import io.circe.{Codec, Decoder, Encoder}
import sourcecode.{Enclosing, File, Line}

import java.io.{PrintStream, PrintWriter}
import java.time.Instant
import scala.util.control.NoStackTrace

/** @param prefix
  *   all log events will start with this string. crucial for the deserializer to know when to deserialize
  */
class jsonEvents(prefix: String) {

  /** Meant for transferring log events between processes */
  case class JsonEvent(formatted: Str, throwable: Option[Th], metadata: Metadata, ctx: Ctx, path: List[String])

  object JsonEvent {
    implicit val strCodec: Codec[Str] =
      Codec.forProduct2[Str, Array[Char], Array[Long]]("chars", "colors") { case (chars, colors) => Str.fromArrays(chars, colors) } { str =>
        (str.getChars, str.getColors)
      }

    implicit val metadataCodec: Codec.AsObject[Metadata] =
      Codec.forProduct5[Metadata, Instant, Int, Int, String, String]("instant", "logLevel", "line", "file", "enclosing") {
        case (instant, logLevel, line, file, enclosing) =>
          new Metadata(instant, LogLevel.unsafeFrom(logLevel), new Line(line), new File(file), new Enclosing(enclosing))
      }(m => (m.instant, m.logLevel.level, m.line.value, m.file.value, m.enclosing.value))

    implicit val codec: Codec[JsonEvent] = semiauto.deriveCodec

    def from[T: Formatter](t: T, throwable: Option[Throwable], metadata: Metadata, ctx: Ctx, path: List[String]): JsonEvent =
      JsonEvent(Formatter[T](t), throwable.map(Th.from), metadata, ctx, path)
  }

  /** For used in called program, so it outputs all log events in json
    */
  final class SerializeLogEvents[U <: Appendable](val underlying: U, val context: Ctx, val path: List[String]) extends TypedLogger[U] {
    import io.circe.syntax.*

    override def apply[T: Formatter](t: => T, throwable: Option[Throwable], metadata: Metadata): Unit = {
      val jsonEvent = JsonEvent.from(t, throwable, metadata, context, path)
      if (
        // old scheme
        jsonEvent.formatted.plainText.startsWith("{") ||
        // new scheme
        jsonEvent.formatted.plainText.startsWith(prefix)
      ) underlying.append(jsonEvent.formatted.plainText + "\n")
      else underlying.append(jsonEvent.asJson.noSpaces + "\n")
      ()
    }

    override def withContext[T: Formatter](key: String, value: T): SerializeLogEvents[U] =
      new SerializeLogEvents(underlying, context + (key -> Formatter(value)), path)

    override def progressMonitor: Option[LoggerFn] = None

    override def withPath(fragment: String): TypedLogger[U] =
      new SerializeLogEvents[U](underlying, context, fragment :: path)
    override val minLogLevel: LogLevel =
      LogLevel.debug
  }

  final class DeserializeLogEvents[U](next: TypedLogger[U]) extends TypedLogger[U] {
    override def apply[T: Formatter](t: => T, throwable: Option[Throwable], metadata: Metadata): Unit = {
      def doLog(plainText: String): Unit =
        decode[JsonEvent](plainText) match {
          case Left(_) => next.apply(t, throwable, metadata)
          case Right(jsonEvent) =>
            val logger1 = jsonEvent.path.foldRight(next) { case (fragment, acc) => acc.withPath(fragment) }
            val logger2 = jsonEvent.ctx.foldRight(logger1) { case ((k, v), acc) => acc.withContext(k, v) }

            logger2.apply(
              jsonEvent.formatted,
              jsonEvent.throwable.map(DeserializedThrowable.apply),
              jsonEvent.metadata
            )(using Formatter.StrFormatter)
        }

      val str = implicitly[Formatter[T]].apply(t)
      // old scheme
      if (str.plainText.startsWith("{")) doLog(str.plainText)
      // new scheme
      else if (str.plainText.startsWith(prefix)) doLog(str.plainText.drop(prefix.length))
      else next.apply(t, throwable, metadata)
    }

    override def withContext[T: Formatter](key: String, value: T): DeserializeLogEvents[U] =
      new DeserializeLogEvents(next.withContext(key, value))

    override def progressMonitor: Option[LoggerFn] = next.progressMonitor

    override def withPath(fragment: String): DeserializeLogEvents[U] =
      new DeserializeLogEvents(next.withPath(fragment))

    override def underlying: U = next.underlying
    override val minLogLevel: LogLevel = next.minLogLevel
  }

  case class DeserializedThrowable(th: Th) extends Throwable with NoStackTrace {
    override def printStackTrace(s: PrintStream): Unit = s.println(th.stackTrace)
    override def printStackTrace(s: PrintWriter): Unit = s.println(th.stackTrace)
    override def getMessage: String = th.message.getOrElse("")
  }

  /** Wrap exceptions in something which is easier to transfer
    */
  case class Th(className: String, message: Option[String], cause: Option[Th], stackTrace: String, suppressed: Array[Th])
      extends Throwable()

  object Th {
    implicit val encoder: Encoder[Th] = semiauto.deriveEncoder
    implicit val decoder: Decoder[Th] = semiauto.deriveDecoder

    def from(th: Throwable): Th =
      Th(
        className = th.getClass.getName,
        message = Option(th.getMessage),
        cause = Option(th.getCause).map(from),
        stackTrace = Throwables.asString(th),
        suppressed = th.getSuppressed.map(from)
      )
  }
}
