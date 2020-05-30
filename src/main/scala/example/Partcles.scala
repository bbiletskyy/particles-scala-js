package example
import java.util.UUID

import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom
import org.scalajs.dom.html

import scala.scalajs.js
import scala.util.Random



@JSExport
object Particles {
  @JSExport
  def main(canvas: html.Canvas): Unit = {
    /*setup*/
    val renderer = canvas.getContext("2d")
      .asInstanceOf[dom.CanvasRenderingContext2D]

    canvas.width = canvas.parentElement.clientWidth
    canvas.height = 400

    //var cfg = Configuration(Set(StaticParticle(Point(100, 100)), ActiveParticle(Point(200, 100))))
    val particles = Configuration.squareStaticMembrane(30)
    val ap = ActiveParticle(Point(1, 1))
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
    //var cfg = Configuration(particles.ps + bi1 + bi2 + ap)
    var cfg = Configuration(particles.ps + bi1 + bi2 + bi3 + bi4 + bi5 + bi6 + ap)
    //var cfg = Configuration(particles.ps + bi1 + bi2 + ap + b1 + b2 + b0 + sp)

    def drawCfg(cg: Configuration): Unit = {
      renderer.clearRect(0, 0, canvas.width, canvas.height)

      for (p: Particle <- cfg.ps) {
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

    //dom.window.setInterval(run _, 20)
    dom.window.setInterval(run _, 2)
  }
}

case class Point(x: Int, y: Int) {
  def +(p: Point) = Point(x + p.x, y + p.y)
  def -(p: Point) = Point(x - p.x, y - p.y)
  def /(d: Int) = Point(x / d, y / d)
  def norm(p: Int): Double = {
    Math.pow(Math.pow(Math.abs(x), p) + Math.pow(Math.abs(y), p), 1/p)
  }
}


trait Particle {
  def transform(): Particle
  def transform(cfg: Configuration): Configuration = cfg
  def energy(cfg: Configuration): Double = cfg.ps.filter(p => p.point == this.point).size.toDouble
  def point: Point
  def kind: String
  def id: String
}

case class StaticParticle(point: Point,  id: String = UUID.randomUUID().toString) extends Particle {
  def kind = "static"
  def transform(): StaticParticle = {
    this.copy()
  }
}

case class ActiveParticle(point: Point, id: String = UUID.randomUUID().toString) extends Particle {
  def step = 2
  def kind = "active"
  def transform(): ActiveParticle = {
    def nextInt(n: Int): Int = Random.nextInt(n) * (if (Random.nextBoolean())  -1 else 1)
    val newPoint = Point(this.point.x + nextInt(step), this.point.y + nextInt(step))
    copy(point = newPoint)
  }

  override def transform(cfg: Configuration): Configuration = {
    val otherParticles = cfg.ps.filterNot(p => p.id == this.id)
    assert(otherParticles.size == cfg.ps.size - 1)
    cfg.copy(ps = otherParticles + this.transform())
  }
}


case class BindingParticle(point: Point, id: String = UUID.randomUUID().toString) extends Particle {
  def step = 2
  def kind = "binding"
  def transform(): BindingParticle = {
    def nextInt(n: Int): Int = Random.nextInt(n) * (if (Random.nextBoolean())  -1 else 1)
    val newPoint = Point(this.point.x + nextInt(step), this.point.y + nextInt(step))
    copy(point = newPoint)
  }

  override def transform(cfg: Configuration): Configuration = {
    val otherParticles = cfg.ps.filterNot(p => p.id == this.id)

    val otherBindingNearbyParticles = otherParticles
      .filter(p => p.isInstanceOf[BindingParticle])
      .filter(p => (p.point - this.point).norm(1) < 2.0)

    val newCfg = if (!otherBindingNearbyParticles.isEmpty) {// bind
      val randIdx = Random.nextInt(otherBindingNearbyParticles.size)
      val randOtherBindingParticle = otherBindingNearbyParticles.toList(randIdx).asInstanceOf[BindingParticle]
      val (me, other) = Bind(this, randOtherBindingParticle)
      val allOtherParticles = cfg.ps.filterNot(p => Set(this .id, other.id).contains(p.id))
      Configuration(allOtherParticles + me + other)
    } else {// move
      cfg.copy(ps = otherParticles + this.transform())
    }
    newCfg
  }
}

case class BondedParticle(point: Point, otherId: String, id: String = UUID.randomUUID().toString) extends Particle {
  def step = 2
  def kind = "bonded"
  def transform(): BondedParticle = {
    def nextInt(n: Int): Int = Random.nextInt(n) * (if (Random.nextBoolean())  -1 else 1)
    val newPoint = Point(this.point.x + nextInt(step), this.point.y + nextInt(step))
    copy(point = newPoint)
  }

  override def energy(cfg: Configuration): Double = {
    val bondParticles = cfg.ps.filter(p => p.id == this.otherId)
    assert(bondParticles.size == 1)
    val distance = (point - bondParticles.head.point).norm(1)
    val bondEnergy = if (distance < 5) 0 else 1
    super.energy(cfg) + bondEnergy
  }
  override def transform(cfg: Configuration): Configuration = {
    val otherParticles = cfg.ps.filterNot(p => p.id == this.id)
    assert(otherParticles.size == cfg.ps.size - 1)
    cfg.copy(ps = otherParticles + this.transform())
  }
}

case class Configuration(ps: Set[Particle]) {
//  def transform(): Configuration = {
//      val activeParticles: Set[Particle] = ps.filter(p => !p.isInstanceOf[StaticParticle])
//      val randomParticle: Particle = activeParticles.toList(Random.nextInt(activeParticles.size))
//      val newCfg = randomParticle.transform(this)
//      if (newCfg.energy() <= this.energy()) newCfg else this
//  }


  def transform(): Configuration = {
    def transform1(): Configuration = {
      val activeParticles: Set[Particle] = ps.filter(p => !p.isInstanceOf[StaticParticle])
      val randomParticle: Particle = activeParticles.toList(Random.nextInt(activeParticles.size))
      val newCfg = randomParticle.transform(this)
      if (newCfg.energy() <= this.energy()) newCfg else this
    }

    var c = this
    for (i <- 0 to 1) {
      c = transform1()
    }
    c
  }

  def energy(): Double = ps.foldLeft(0.0)((e, p) => e + p.energy(this))
}

object Configuration {
  def squareStaticMembrane(sideSize: Int): Configuration = {
    var ps = Set.empty[Particle]
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