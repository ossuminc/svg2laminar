Global / onChangedBuildSource := ReloadOnSourceChanges
(Global / excludeLintKeys) ++= Set(mainClass)

enablePlugins(OssumIncPlugin)

lazy val developers: List[Developer] = List(
  Developer(
    id = "reid-spencer",
    "Reid Spencer",
    "reid.spencer@ossuminc.com",
    url("https://github.com/reid-spencer")
  )
)

lazy val svg2laminar = Root(
  ghRepoName = "svg2laminar",
  ghOrgName = "ossuminc",
  orgPackage = "com.ossuminc.svg2laminar",
  orgName = "Ossum Inc.",
  orgPage = url("https://www.ossuminc.com/"),
  startYr = 2024,
  devs = developers,
  spdx = "Apache-2.0"
).configure(With.GithubPublishing)
  .configure(With.typical, With.header)
  .configure(With.noMiMa)
  .configure(
    With.packagingUniversal(
      maintainerEmail = "reid@ossuminc.com",
      pkgName = "riddlc",
      pkgSummary = "Compiler for RIDDL language, Universal packaging",
      pkgDescription = ""
    )
  )
  .settings(
    description := "A translater of svg files into Laminar svg syntax",
    maintainer := "reid@ossuminc.com",
    mainClass := Option("com.ossuminc.svg2Laminar.Svg2Laminar"),
    libraryDependencies ++= Seq(Dep.sconfig, Dep.scopt, Dep.xml)
  )
