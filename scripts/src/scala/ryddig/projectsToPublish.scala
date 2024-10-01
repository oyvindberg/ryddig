package ryddig

import bleep.model

object projectsToPublish {
  // will publish these with dependencies
  def include(crossName: model.CrossProjectName): Boolean = {
    val name = crossName.name.value
    name.startsWith("ryddig") && !name.endsWith("-test")
  }
}
