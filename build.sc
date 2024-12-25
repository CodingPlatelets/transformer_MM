// import Mill dependency
import mill._
import mill.define.Sources
import mill.modules.Util
import mill.scalalib.TestModule.ScalaTest
import scalalib._
// support BSP
import mill.bsp._

val defaultScalaVersion = "2.13.12"
def defaultVersions = Map(
  "chisel" -> ivy"org.chipsalliance::chisel:6.2.0",
  "chisel-plugin" -> ivy"org.chipsalliance:::chisel-plugin:6.2.0",
  "chiseltest" -> ivy"edu.berkeley.cs::chiseltest:6.0.0"
)

def extendDependencies = Map(
  "hardfloat" -> ivy"edu.berkeley.cs::hardfloat:1.5-SNAPSHOT"
)

trait HasChisel extends SbtModule {
  def chiselModule: Option[ScalaModule] = None

  def chiselPluginJar: T[Option[PathRef]] = None

  def chiselIvy: Option[Dep] = Some(defaultVersions("chisel"))

  def chiselPluginIvy: Option[Dep] = Some(defaultVersions("chisel-plugin"))

  override def scalaVersion = defaultScalaVersion

  override def scalacOptions = super.scalacOptions() ++
    Agg("-language:reflectiveCalls", "-Ymacro-annotations", "-Ytasty-reader")

  override def ivyDeps = super.ivyDeps() ++ Agg(chiselIvy.get)

  override def scalacPluginIvyDeps = super.scalacPluginIvyDeps() ++ Agg(chiselPluginIvy.get)
}

object fputil extends HasChisel {
  override def millSourcePath = os.pwd / "dependencies" / "fputil" 
}

trait transformer_MMModule extends ScalaModule {
  def fputilModule: ScalaModule

  override def moduleDeps = super.moduleDeps ++ Seq(
    fputilModule
  )

  val resourcesPATH = os.pwd.toString() + "/src/main/resources"
}

object trans extends transformer_MMModule with HasChisel { m =>
  def fputilModule = fputil

  override def millSourcePath = os.pwd
  def unmanagedClasspath = T {
    if (!os.exists(millSourcePath / "lib")) Agg()
    else Agg.from(os.list(millSourcePath / "lib").map(PathRef(_)))
  }
  override def scalaVersion = "2.13.12"
  override def scalacOptions = Seq(
    "-language:reflectiveCalls",
    "-deprecation",
    "-feature",
    "-Xcheckinit"
  )
  override def forkArgs = Seq("-Xmx8G", "-Xss256m")
  // def mainClass = Some("vitiskernel.VitisRTLKernelVerilog")
  def mainClass = Some("kernel.NewFeatureTest")
  override def ivyDeps = super.ivyDeps() ++ Agg(
    defaultVersions("chiseltest"),
    extendDependencies("hardfloat")
  )

  object test extends SbtTests with TestModule.ScalaTest {
    override def forkArgs = Seq("-Xmx8G", "-Xss256m")

    override def ivyDeps = super.ivyDeps() ++ Agg(
      defaultVersions("chiseltest")
    )

    override def scalacOptions = super.scalacOptions() ++ Agg("-deprecation", "-feature")

    val resourcesPATH = os.pwd.toString() + "/src/main/resources"
    val envPATH = sys.env("PATH") + ":" + resourcesPATH

    override def forkEnv = Map("PATH" -> envPATH)
  }
}
