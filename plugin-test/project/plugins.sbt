lazy val plugins = project.in(file(".")).dependsOn(ProjectRef(file("../.."), "clientCodegenSbt"))
