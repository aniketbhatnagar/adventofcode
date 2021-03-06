logLevel := Level.Warn

addSbtPlugin("org.scalastyle" %% "scalastyle-sbt-plugin" % "0.7.0")

addSbtPlugin("de.johoop" % "cpd4sbt" % "1.1.5")

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.3.3")

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.0")

resolvers += "sonatype-releases" at "https://oss.sonatype.org/content/repositories/releases/"
