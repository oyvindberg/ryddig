package ryddig

import java.io.{PrintStream, Writer}
import java.util.concurrent.atomic.AtomicBoolean

/** We do some acrobatics to make sure that this supertype of `TypedLogger` can expose all the same operations in an untyped form.
  */
sealed trait Logger extends LoggerFn with LogActions {
  def context: Ctx
  def path: List[String]
  val minLogLevel: LogLevel
}

object Logger {
  implicit class LoggerOps(l: Logger) {
    def withContext[T: Formatter](key: String, value: T): Logger =
      l match {
        case l: TypedLogger[t] => l.withContext(key, value)
      }

    def withOptContext[T: Formatter](key: String, maybeValue: Option[T]): Logger =
      maybeValue match {
        case Some(value) => withContext(key, value)
        case None        => l
      }

    def withPath(fragment: String): Logger =
      l match {
        case l: TypedLogger[t] => l.withPath(fragment)
      }

    def withMinLogLevel(minLogLevel: LogLevel): Logger =
      l match {
        case l: TypedLogger[t] => l.withMinLogLevel(minLogLevel)
      }

    def zipWith(other: Logger): Logger =
      (l, other) match {
        case (l1: TypedLogger[t1], l2: TypedLogger[t2]) => l1.zipWith(l2)
      }

    def maybeZipWith(other: Option[Logger]): Logger =
      (l, other) match {
        case (l1: TypedLogger[t1], Some(l2: TypedLogger[t2])) => l1.maybeZipWith(Some(l2))
        case (l1: TypedLogger[t1], None)                      => l1.maybeZipWith(None)
      }

    def syncAccess(on: Object): Logger =
      l match {
        case l: TypedLogger[t] => l.syncAccess(on)
      }
  }
}

trait TypedLogger[+Underlying] extends Logger {
  def underlying: Underlying

  def withContext[T: Formatter](key: String, value: T): TypedLogger[Underlying]

  def withPath(fragment: String): TypedLogger[Underlying]

  @deprecated("this is only here until bleep has a proper thing to render UI like tui.", "0.1.0")
  def progressMonitor: Option[LoggerFn]

  final def withOptContext[T: Formatter](key: String, maybeValue: Option[T]): TypedLogger[Underlying] =
    maybeValue match {
      case Some(value) => withContext(key, value)
      case None        => this
    }

  final def zipWith[U2](other: TypedLogger[U2]): TypedLogger[(Underlying, U2)] =
    new TypedLogger.Zipped(this, other)

  final def maybeZipWith[U2](other: Option[TypedLogger[U2]]): TypedLogger[(Underlying, Option[U2])] =
    new TypedLogger.MaybeZipped(this, other)

  final def withMinLogLevel(level: LogLevel): TypedLogger[Underlying] =
    new TypedLogger.MinLogLevel(this, level)

  final def map[U2](f: Underlying => U2): TypedLogger[U2] =
    new TypedLogger.Mapped(this, f)

  final def syncAccess(on: Object): TypedLogger[Underlying] =
    new TypedLogger.Synchronized(this, on)
}

object TypedLogger {
  private[ryddig] class Store() {
    private var reversed: List[Stored] = Nil

    def store(s: Stored): Unit =
      reversed = s :: reversed

    def normal: Array[Stored] =
      reversed.toArray.reverse
  }

  private[ryddig] final class StoringLogger(store: Store, override val context: Ctx, override val path: List[String])
      extends TypedLogger[Array[Stored]] {

    override def apply[T: Formatter](t: => T, throwable: Option[Throwable], metadata: Metadata): Unit =
      store.store(Stored(Formatter(t), throwable, metadata, context, path))

    override def withContext[T: Formatter](key: String, value: T): StoringLogger =
      new StoringLogger(store, context + (key -> Formatter(value)), path)

    override def withPath(fragment: String): TypedLogger[Array[Stored]] =
      new StoringLogger(store, context, fragment :: path)

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
      override val context: Ctx,
      override val path: List[String]
  ) extends TypedLogger[U] { self =>
    override def apply[T: Formatter](t: => T, throwable: Option[Throwable], metadata: Metadata): Unit = {
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
      override val context: Ctx,
      override val path: List[String],
      disableProgress: Boolean,
      lastWasProgress: AtomicBoolean = new AtomicBoolean(false) // need to share this across instances after `withContext`
  ) extends TypedLogger[U] { self =>

    val CleanCurrentLine = "\u001b[K"

    override def apply[T: Formatter](t: => T, throwable: Option[Throwable], metadata: Metadata): Unit = {
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

    override def progressMonitor: Option[LoggerFn] =
      if (disableProgress) None
      else
        Some {
          new LoggerFn {
            override def apply[T: Formatter](text: => T, throwable: Option[Throwable], metadata: Metadata): Unit = {
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

    override def apply[T: Formatter](t: => T, throwable: Option[Throwable], metadata: Metadata): Unit = {
      one.apply(t, throwable, metadata)
      two.apply(t, throwable, metadata)
    }

    override def withContext[T: Formatter](key: String, value: T): Zipped[U1, U2] =
      new Zipped(one.withContext(key, value), two.withContext(key, value))

    override def withPath(fragment: String): Zipped[U1, U2] =
      new Zipped(one.withPath(fragment), two.withPath(fragment))

    override def progressMonitor: Option[LoggerFn] =
      one.progressMonitor.orElse(two.progressMonitor)

    override val minLogLevel: LogLevel =
      one.minLogLevel.min(two.minLogLevel)

    override def context: Ctx =
      one.context

    override def path: List[String] =
      one.path
  }

  private[ryddig] final class MaybeZipped[U1, U2](one: TypedLogger[U1], two: Option[TypedLogger[U2]])
      extends TypedLogger[(U1, Option[U2])] {
    override def underlying: (U1, Option[U2]) =
      (one.underlying, two.map(_.underlying))

    override def apply[T: Formatter](t: => T, throwable: Option[Throwable], metadata: Metadata): Unit = {
      one.apply(t, throwable, metadata)
      two match {
        case Some(two) => two.apply(t, throwable, metadata)
        case None      => ()
      }
    }

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

    override def context: Ctx =
      one.context

    override def path: List[String] =
      one.path
  }

  private[ryddig] final class MinLogLevel[U](wrapped: TypedLogger[U], val minLogLevel: LogLevel) extends TypedLogger[U] {
    override def underlying: U = wrapped.underlying

    override def apply[T: Formatter](t: => T, throwable: Option[Throwable], m: Metadata): Unit =
      if (m.logLevel.level >= minLogLevel.level) {
        wrapped.apply(t, throwable, m)
      }

    override def withContext[T: Formatter](key: String, value: T): MinLogLevel[U] =
      new MinLogLevel[U](wrapped.withContext(key, value), minLogLevel)

    override def withPath(fragment: String): MinLogLevel[U] =
      new MinLogLevel[U](wrapped.withPath(fragment), minLogLevel)

    override def progressMonitor: Option[LoggerFn] =
      wrapped.progressMonitor

    override def context: Ctx =
      wrapped.context

    override def path: List[String] =
      wrapped.path
  }

  private[ryddig] final class Mapped[U, UU](wrapped: TypedLogger[U], f: U => UU) extends TypedLogger[UU] {
    override def underlying: UU = f(wrapped.underlying)

    override def apply[T: Formatter](t: => T, throwable: Option[Throwable], m: Metadata): Unit =
      wrapped.apply(t, throwable, m)

    override def withContext[T: Formatter](key: String, value: T): Mapped[U, UU] =
      new Mapped(wrapped.withContext(key, value), f)

    override def withPath(fragment: String): Mapped[U, UU] =
      new Mapped[U, UU](wrapped.withPath(fragment), f)

    override def progressMonitor: Option[LoggerFn] =
      wrapped.progressMonitor

    override val minLogLevel: LogLevel =
      wrapped.minLogLevel

    override def context: Ctx =
      wrapped.context

    override def path: List[String] =
      wrapped.path
  }

  private[ryddig] final class Synchronized[U](wrapped: TypedLogger[U], on: Object) extends TypedLogger[U] {
    override def underlying: U = wrapped.underlying

    override def apply[T: Formatter](t: => T, throwable: Option[Throwable], m: Metadata): Unit =
      on.synchronized(wrapped.apply(t, throwable, m))

    override def withContext[T: Formatter](key: String, value: T): Synchronized[U] =
      new Synchronized(wrapped.withContext(key, value), on)

    override def withPath(fragment: String): Synchronized[U] =
      new Synchronized(wrapped.withPath(fragment), on)

    override def progressMonitor: Option[LoggerFn] =
      wrapped.progressMonitor

    override val minLogLevel: LogLevel =
      wrapped.minLogLevel

    override def context: Ctx =
      wrapped.context

    override def path: List[String] =
      wrapped.path
  }

  object DevNull extends TypedLogger[Unit] {
    override def underlying: Unit = ()
    override def withContext[T: Formatter](key: String, value: T): DevNull.type = this
    override def withPath(fragment: String): DevNull.type = this
    override def apply[T: Formatter](text: => T, throwable: Option[Throwable], metadata: Metadata): Unit = ()
    override def progressMonitor: Option[LoggerFn] = None
    override val minLogLevel: LogLevel = LogLevel.error
    override def context: Ctx = Loggers.emptyContext
    override def path: List[String] = Nil
  }
}
