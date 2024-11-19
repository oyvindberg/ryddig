package ryddig

import fansi.Str

case class Stored(message: Str, throwable: Option[Throwable], metadata: Metadata, ctx: Ctx, path: List[String])
