package ryddig

import fansi.Str

import java.io.{PrintStream, Writer}
import java.util.concurrent.atomic.AtomicBoolean

trait TypedLogger[Underlying] extends LoggerFn {
  val minLogLevel: LogLevel
  
  def underlying: Underlying

  def withContext[T: Formatter](key: String, value: T): TypedLogger[Underlying]

  def withPath(fragment: String): TypedLogger[Underlying]

  def progressMonitor: Option[LoggerFn]

  final def withOptContext[T: Formatter](key: String, maybeValue: Option[T]): TypedLogger[Underlying] =
    maybeValue match {
      case Some(value) => withContext(key, value)
      case None        => this
    }

  final def zipWith[UU](other: TypedLogger[UU]): TypedLogger[(Underlying, UU)] =
    new TypedLogger.Zipped(this, other)

  final def maybeZipWith[UU](other: Option[TypedLogger[UU]]): TypedLogger[(Underlying, Option[UU])] =
    new TypedLogger.MaybeZipped(this, other)

  final def withMinLogLevel(minLogLevel: LogLevel): TypedLogger[Underlying] =
    new TypedLogger.MinLogLevel(this, minLogLevel)

  final def untyped: TypedLogger[Unit] =
    new TypedLogger.Mapped[Underlying, Unit](this, _ => ())

  final def syncAccess: TypedLogger[Underlying] =
    new TypedLogger.Synchronized(this)
}

object TypedLogger {

  case class Stored(message: Str, throwable: Option[Throwable], metadata: Metadata, ctx: Ctx, path: List[String])

  private[ryddig] class Store() {
    private var reversed: List[Stored] = Nil

    def store(s: Stored): Unit =
      reversed = s :: reversed

    def normal: Array[Stored] =
      reversed.toArray.reverse
  }

  private[ryddig] final class StoringLogger(store: Store, val ctx: Ctx, path: List[String]) extends TypedLogger[Array[Stored]] {

    override def log[T: Formatter](t: => T, throwable: Option[Throwable], metadata: Metadata): Unit =
      store.store(Stored(Formatter(t), throwable, metadata, ctx, path))

    override def withContext[T: Formatter](key: String, value: T): StoringLogger =
      new StoringLogger(store, ctx + (key -> Formatter(value)), path)

    override def withPath(fragment: String): TypedLogger[Array[Stored]] =
      new StoringLogger(store, ctx, fragment :: path)

    override def underlying: Array[Stored] =
      store.normal

    override def progressMonitor: Option[LoggerFn] =
      None

    override val minLogLevel: LogLevel =
      LogLevel.debug
  }

  private[ryddig] final class WriterLogger[U <: Writer](
      val underlying: U,
      flush: Boolean,
      pattern: Pattern,
      val context: Ctx,
      val path: List[String]
  ) extends TypedLogger[U] { self =>

    override def log[T: Formatter](t: => T, throwable: Option[Throwable], metadata: Metadata): Unit = {
      val formatted = pattern(t, throwable, metadata, context, path)
      underlying.append(formatted.render + "\n")
      if (flush) {
        underlying.flush()
      }
      ()
    }

    override def withContext[T: Formatter](key: String, value: T): WriterLogger[U] =
      new WriterLogger(underlying, flush, pattern, context + (key -> Formatter(value)), path)

    override def progressMonitor: Option[LoggerFn] = None

    override def withPath(fragment: String): WriterLogger[U] =
      new WriterLogger(underlying, flush, pattern, context, fragment :: path)

    override val minLogLevel: LogLevel = LogLevel.debug
  }

  private[ryddig] final class ConsoleLogger[U <: PrintStream](
      val underlying: U,
      pattern: Pattern,
      val context: Ctx,
      path: List[String],
      disableProgress: Boolean,
      lastWasProgress: AtomicBoolean = new AtomicBoolean(false) // need to share this across instances after `withContext`
  ) extends TypedLogger[U] {

    val CleanCurrentLine = "\u001b[K"

    override def log[T: Formatter](t: => T, throwable: Option[Throwable], metadata: Metadata): Unit = {
      val formatted = pattern(t, throwable, metadata, context, path)
      if (lastWasProgress.get()) {
        underlying.println(CleanCurrentLine + formatted.render)
      } else {
        underlying.println(formatted.render)
      }

      lastWasProgress.set(false)
      ()
    }

    override def withContext[T: Formatter](key: String, value: T): ConsoleLogger[U] =
      new ConsoleLogger(underlying, pattern, context + (key -> Formatter(value)), path, disableProgress, lastWasProgress)

    override def withPath(fragment: String): ConsoleLogger[U] =
      new ConsoleLogger(underlying, pattern, context, fragment :: path, disableProgress, lastWasProgress)

    // todo: this is only here until we have a proper thing to render UI like tui.
    override def progressMonitor: Option[LoggerFn] =
      if (disableProgress) None
      else
        Some {
          new LoggerFn {
            override def log[T: Formatter](text: => T, throwable: Option[Throwable], metadata: Metadata): Unit = {
              val formatted = pattern(text, throwable, metadata, context, path)
              if (lastWasProgress.get()) {
                underlying.print(CleanCurrentLine + formatted.render + "\r")
                ()
              } else {
                underlying.print(formatted.render + "\r")
                lastWasProgress.set(true)
              }
            }
          }
        }

    override val minLogLevel: LogLevel =
      LogLevel.debug
  }

  private[ryddig] final class Zipped[U1, U2](one: TypedLogger[U1], two: TypedLogger[U2]) extends TypedLogger[(U1, U2)] {
    override def underlying: (U1, U2) =
      (one.underlying, two.underlying)

    private val both = one.and(two)

    override def log[T: Formatter](t: => T, throwable: Option[Throwable], metadata: Metadata): Unit =
      both.log(t, throwable, metadata)

    override def withContext[T: Formatter](key: String, value: T): Zipped[U1, U2] =
      new Zipped(one.withContext(key, value), two.withContext(key, value))

    override def withPath(fragment: String): Zipped[U1, U2] =
      new Zipped(one.withPath(fragment), two.withPath(fragment))

    override def progressMonitor: Option[LoggerFn] =
      List(one.progressMonitor, two.progressMonitor).flatten.reduceOption(_.and(_))

    override val minLogLevel: LogLevel =
      one.minLogLevel.min(two.minLogLevel)
  }

  private[ryddig] final class MaybeZipped[U1, U2](one: TypedLogger[U1], two: Option[TypedLogger[U2]])
      extends TypedLogger[(U1, Option[U2])] {
    override def underlying: (U1, Option[U2]) =
      (one.underlying, two.map(_.underlying))

    private val both =
      two match {
        case Some(two) => one.and(two)
        case None      => one
      }

    override def log[T: Formatter](t: => T, throwable: Option[Throwable], metadata: Metadata): Unit =
      both.log(t, throwable, metadata)

    override def withContext[T: Formatter](key: String, value: T): MaybeZipped[U1, U2] =
      new MaybeZipped(one.withContext(key, value), two.map(_.withContext(key, value)))

    override def withPath(fragment: String): MaybeZipped[U1, U2] =
      new MaybeZipped(one.withPath(fragment), two.map(_.withPath(fragment)))

    override def progressMonitor: Option[LoggerFn] =
      one.progressMonitor match {
        case some @ Some(_) => some
        case None =>
          two match {
            case Some(two) => two.progressMonitor
            case None      => None
          }
      }

    override val minLogLevel: LogLevel =
      two match {
        case Some(two) => one.minLogLevel.min(two.minLogLevel)
        case None      => one.minLogLevel
      }
  }

  private[ryddig] final class MinLogLevel[U](wrapped: TypedLogger[U], val minLogLevel: LogLevel) extends TypedLogger[U] {
    override def underlying: U = wrapped.underlying

    override def log[T: Formatter](t: => T, throwable: Option[Throwable], m: Metadata): Unit =
      if (m.logLevel.level >= minLogLevel.level) {
        wrapped.log(t, throwable, m)
      }

    override def withContext[T: Formatter](key: String, value: T): MinLogLevel[U] =
      new MinLogLevel[U](wrapped.withContext(key, value), minLogLevel)

    override def withPath(fragment: String): MinLogLevel[U] =
      new MinLogLevel[U](wrapped.withPath(fragment), minLogLevel)

    override def progressMonitor: Option[LoggerFn] =
      wrapped.progressMonitor
  }

  private[ryddig] final class Mapped[U, UU](wrapped: TypedLogger[U], f: U => UU) extends TypedLogger[UU] {
    override def underlying: UU = f(wrapped.underlying)

    override def log[T: Formatter](t: => T, throwable: Option[Throwable], m: Metadata): Unit =
      wrapped.log(t, throwable, m)

    override def withContext[T: Formatter](key: String, value: T): Mapped[U, UU] =
      new Mapped(wrapped.withContext(key, value), f)

    override def withPath(fragment: String): Mapped[U, UU] =
      new Mapped[U, UU](wrapped.withPath(fragment), f)

    override def progressMonitor: Option[LoggerFn] =
      wrapped.progressMonitor

    override val minLogLevel: LogLevel =
      wrapped.minLogLevel
  }

  private[ryddig] final class Synchronized[U](wrapped: TypedLogger[U]) extends TypedLogger[U] {
    override def underlying: U = wrapped.underlying

    override def log[T: Formatter](t: => T, throwable: Option[Throwable], m: Metadata): Unit =
      this.synchronized(wrapped.log(t, throwable, m))

    override def withContext[T: Formatter](key: String, value: T): Synchronized[U] =
      new Synchronized(wrapped.withContext(key, value))

    override def withPath(fragment: String): Synchronized[U] =
      new Synchronized(wrapped.withPath(fragment))

    override def progressMonitor: Option[LoggerFn] =
      wrapped.progressMonitor

    override val minLogLevel: LogLevel =
      wrapped.minLogLevel
  }

  object DevNull extends TypedLogger[Unit] {
    override def underlying: Unit = ()
    override def withContext[T: Formatter](key: String, value: T): DevNull.type = this
    override def withPath(fragment: String): DevNull.type = this
    override def log[T: Formatter](text: => T, throwable: Option[Throwable], metadata: Metadata): Unit = ()
    override def progressMonitor: Option[LoggerFn] = None
    override val minLogLevel: LogLevel = LogLevel.error
  }
}
