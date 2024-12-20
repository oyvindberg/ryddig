package ryddig

import java.io.{BufferedWriter, Flushable, PrintStream, Writer}
import java.nio.file.{Files, Path, StandardOpenOption}

object Loggers {
  private[ryddig] val emptyContext: Ctx = Map.empty

  // this is a resource since we absolutely should flush it before we exit
  def stdout(pattern: Pattern, disableProgress: Boolean, ctx: Ctx = emptyContext): LoggerResource[PrintStream] =
    LoggerResource.flushable {
      new TypedLogger.ConsoleLogger(System.out, pattern, ctx, Nil, disableProgress)
    }

  def stdoutNoFlush(pattern: Pattern, disableProgress: Boolean, ctx: Ctx = emptyContext): TypedLogger[PrintStream] =
    new TypedLogger.ConsoleLogger(System.out, pattern, ctx, Nil, disableProgress)

  // this is unbuffered, so I don't think there is any reason to care further
  def stderr(pattern: Pattern, ctx: Ctx = emptyContext): TypedLogger[PrintStream] =
    new TypedLogger.ConsoleLogger(System.err, pattern, ctx, Nil, disableProgress = true)

  // this is a resource since we absolutely should close/flush it before we exit
  def path(logFile: Path, pattern: Pattern, ctx: Ctx = emptyContext): LoggerResource[BufferedWriter] =
    LoggerResource.autoCloseable {
      Files.createDirectories(logFile.getParent)
      val w = Files.newBufferedWriter(logFile, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)
      writer(w, flush = true, pattern, ctx)
    }

  // wrap in TypedLoggerResource if you need it flushed/closed
  def writer[A <: Writer](writer: A, flush: Boolean, pattern: Pattern, ctx: Ctx = emptyContext): TypedLogger[A] =
    new TypedLogger.WriterLogger(writer, flush, pattern, ctx, Nil)

  def storing(ctx: Ctx = emptyContext): TypedLogger[Array[Stored]] =
    new TypedLogger.StoringLogger(new TypedLogger.Store, ctx, Nil)

  def printJsonStream[U <: Flushable & Appendable](prefix: String, to: U, ctx: Ctx = emptyContext): TypedLogger[U] = {
    val configured = new jsonEvents(prefix)
    new configured.SerializeLogEvents[U](to, ctx, Nil)
  }

  def decodeJsonStream[U](prefix: String, next: TypedLogger[U]): TypedLogger[U] = {
    val configured = new jsonEvents(prefix)
    new configured.DeserializeLogEvents(next)
  }
}
