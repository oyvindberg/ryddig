package bleep
package scripts

import bleep.nosbt.InteractionService
import bleep.packaging.{CoordinatesFor, PackagedLibrary, PublishLayout, packageLibraries}
import bleep.plugin.cirelease.CiReleasePlugin
import bleep.plugin.dynver.DynVerPlugin
import bleep.plugin.pgp.PgpPlugin
import bleep.plugin.sonatype.Sonatype
import coursier.Info

import scala.collection.immutable.SortedMap

object Publish extends BleepScript("Publish") {
  val groupId = "com.olvind.ryddig"

  def run(started: Started, commands: Commands, args: List[String]): Unit = {
    commands.compile(started.build.explodedProjects.keys.toList)

    val dynVer = new DynVerPlugin(baseDirectory = started.buildPaths.buildDir.toFile, dynverSonatypeSnapshots = true)
    val pgp = new PgpPlugin(
      logger = started.logger,
      maybeCredentials = None,
      interactionService = InteractionService.DoesNotMaskYourPasswordExclamationOneOne
    )
    val sonatype = new Sonatype(
      logger = started.logger,
      sonatypeBundleDirectory = started.buildPaths.dotBleepDir / "sonatype-bundle",
      sonatypeProfileName = groupId,
      bundleName = "ryddig",
      version = dynVer.version,
      sonatypeCredentialHost = Sonatype.sonatype01
    )
    val ciRelease = new CiReleasePlugin(started.logger, sonatype, dynVer, pgp)

    started.logger.info(dynVer.version)

    val info = Info(
      "Log smarter, not harder",
      "https://github.com/oyvindberg/ryddig/",
      List(
        Info.Developer(
          "oyvindberg",
          "Øyvind Raddum Berg",
          "https://github.com/oyvindberg"
        )
      ),
      publication = None,
      scm = CiReleasePlugin.inferScmInfo,
      licenseInfo = List(
        Info.License(
          "MIT",
          Some("http://opensource.org/licenses/MIT"),
          distribution = Some("repo"),
          comments = None
        )
      )
    )

    val packagedLibraries: SortedMap[model.CrossProjectName, PackagedLibrary] =
      packageLibraries(
        started,
        coordinatesFor = CoordinatesFor.Default(groupId = groupId, version = dynVer.version),
        shouldInclude = _ => true,
        publishLayout = PublishLayout.Maven(info)
      )

    val files: Map[RelPath, Array[Byte]] =
      packagedLibraries.flatMap { case (_, PackagedLibrary(_, files)) => files.all }

    files.foreach { case (path, bytes) =>
      started.logger.withContext("path", path.asString).withContext("bytes.length", bytes.length).debug("will publish")
    }
    ciRelease.ciRelease(files)
  }
}
