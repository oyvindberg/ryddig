import fansi.Str

package object ryddig {
  type Ctx = Map[String, Str]
  type Logger = TypedLogger[Unit]
  val Logger = TypedLogger
  type LoggerResource = TypedLoggerResource[Unit]
  val LoggerResource = TypedLoggerResource
}
