package example
import java.util.UUID

import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom
import org.scalajs.dom.html

import scala.scalajs.js
import scala.util.Random



@JSExport
object ParticlesApp {
  @JSExport
  def main(canvas: html.Canvas): Unit = {
    /*setup*/
    val renderer = canvas.getContext("2d")
      .asInstanceOf[dom.CanvasRenderingContext2D]

    canvas.width = canvas.parentElement.clientWidth
    canvas.height = 400

    //var cfg = Configuration(Set(StaticParticle(Point(100, 100)), ActiveParticle(Point(200, 100))))
    val particles = Configuration.squareStaticMembrane(30)
    val ap = FloatingParticle(Point(1, 1))
//    val sp = StaticParticle(Point(10, 10))
//    val b0 = BondedParticle(Point(16, 15), sp.id)
//    val (b1, b2) = Bind(BindingParticle(Point(10, 10)), BindingParticle(Point(11, 10)))
    val bi1 = BindingParticle(Point(5, 5))
    val bi2 = BindingParticle(Point(10, 10))
    val bi3 = BindingParticle(Point(15, 15))
    val bi4 = BindingParticle(Point(20, 20))
    val bi5 = BindingParticle(Point(10, 10))
    val bi6 = BindingParticle(Point(11, 11))


    //var cfg = Configuration(particles.ps + ap)
    var cfg = Configuration(particles.ps + bi1 + bi2 + ap)
    //var cfg = Configuration(particles.ps + bi1 + bi2 + bi3 + bi4 + bi5 + bi6 + ap)
    //var cfg = Configuration(particles.ps + bi1 + bi2 + ap + b1 + b2 + b0 + sp)

    def drawCfg(cg: Configuration): Unit = {
      renderer.clearRect(0, 0, canvas.width, canvas.height)

      for (p: AbstractParticle <- cfg.ps) {
        if (p.kind == "static") renderer.fillStyle = "black"
        else if (p.kind == "active") renderer.fillStyle = "red"
        else if (p.kind == "binding") renderer.fillStyle = "orange"
        else if (p.kind == "bonded") renderer.fillStyle = "darkgreen"
        else renderer.fillStyle = "pink"
        val particleSize = 5
        renderer.fillRect(p.point.x * particleSize, p.point.y * particleSize, particleSize, particleSize)
      }
    }

    def run() = {
      cfg = cfg.transform()
      drawCfg(cfg)
    }

    dom.window.setInterval(run _, 20)
  }
}



case class Configuration(ps: Set[AbstractParticle]) {
  def transform(): Configuration = {
    def update(cfg: Configuration): Configuration = {
      val activeParticles: Set[AbstractParticle] = cfg.ps.filter(p => !p.isInstanceOf[StaticParticle])
      val randomParticle: AbstractParticle = activeParticles.toList(Random.nextInt(activeParticles.size))
      val newCfg = randomParticle.transform(cfg)
      if (newCfg.energy <= cfg.energy) newCfg else cfg
    }

    // do update iters times
    val iters = ps.filter(!_.isInstanceOf[StaticParticle]).size
    (1 to iters).foldLeft(this)((c, _) => update(c))

  }

  def energy(): Double = ps.foldLeft(0.0)((e, p) => e + p.energy(this))
}

object Configuration {
  def squareStaticMembrane(sideSize: Int): Configuration = {
    var ps = Set.empty[AbstractParticle]
    for {i <- 0 to sideSize -1
         j <- 0 to sideSize -1 } {

      if (i == 0 || j == 0 || i == sideSize - 1 || j == sideSize - 1)
        ps = ps + StaticParticle(Point(i, j))
    }
    Configuration(ps)
  }
}

object Bind {
  def apply(a: BindingParticle, b: BindingParticle): (BondedParticle, BondedParticle) = {
    (BondedParticle(a.point, b.id, a.id), BondedParticle(b.point, a.id, b.id))
  }
}