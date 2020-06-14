package example

import java.util.UUID

import scala.util.Random

case class Point(x: Int, y: Int) {
  def +(p: Point) = Point(x + p.x, y + p.y)
  def -(p: Point) = Point(x - p.x, y - p.y)
  def /(d: Int) = Point(x / d, y / d)
  def norm(p: Int): Double = {
    Math.pow(Math.pow(Math.abs(x), p) + Math.pow(Math.abs(y), p), 1/p)
  }
}

trait Particle {
  def transform(cfg: Configuration): Configuration
  def energy(cfg: Configuration): Double
  def point: Point
  def kind: String
  def id: String
}

abstract class AbstractParticle extends Particle {
  def transform(cfg: Configuration): Configuration = cfg
  def energy(cfg: Configuration): Double = cfg.ps.filter(p => p.point == this.point).size.toDouble

}


case class StaticParticle(point: Point, id: String = UUID.randomUUID().toString) extends AbstractParticle {
  def kind = "static"
  def transform(): StaticParticle = {
    this.copy()
  }
}

case class FloatingParticle(point: Point, id: String = UUID.randomUUID().toString) extends AbstractParticle {
  def step = 2
  def kind = "active"
  def float(): FloatingParticle = {
    def nextInt(n: Int): Int = Random.nextInt(n) * (if (Random.nextBoolean())  -1 else 1)
    val newPoint = Point(this.point.x + nextInt(step), this.point.y + nextInt(step))
    copy(point = newPoint)
  }

  override def transform(cfg: Configuration): Configuration = {
    val otherParticles = cfg.ps.filterNot(p => p.id == this.id)
    assert(otherParticles.size == cfg.ps.size - 1)
    cfg.copy(ps = otherParticles + this.float())
  }
}


case class BindingParticle(point: Point, id: String = UUID.randomUUID().toString) extends AbstractParticle {
  def step = 2
  def kind = "binding"
  def bindingDistance = 2.0

  def float(): BindingParticle = {
    def nextInt(n: Int): Int = Random.nextInt(n) * (if (Random.nextBoolean())  -1 else 1)
    val newPoint = Point(this.point.x + nextInt(step), this.point.y + nextInt(step))
    copy(point = newPoint)
  }


  override def transform(cfg: Configuration): Configuration = {
    val otherParticles = cfg.ps.filterNot(p => p.id == this.id)

    val otherBindingNearbyParticles = otherParticles
      .filter(p => p.isInstanceOf[BindingParticle])
      .filter(p => (p.point - this.point).norm(1) < bindingDistance)

    val newCfg = if (!otherBindingNearbyParticles.isEmpty) {// bind
    val randIdx = Random.nextInt(otherBindingNearbyParticles.size)
      val randOtherBindingParticle = otherBindingNearbyParticles.toList(randIdx).asInstanceOf[BindingParticle]
      val (me, other) = Bind(this, randOtherBindingParticle)
      val allOtherParticles = cfg.ps.filterNot(p => Set(this .id, other.id).contains(p.id))
      Configuration(allOtherParticles + me + other)
    } else {// move
      cfg.copy(ps = otherParticles + this.float())
    }
    newCfg
  }
}

case class BondedParticle(point: Point, otherId: String, id: String = UUID.randomUUID().toString) extends AbstractParticle {
  def step = 2
  def kind = "bonded"
  def bondLength = 2

  def float(): BondedParticle = {
    def nextInt(n: Int): Int = Random.nextInt(n) * (if (Random.nextBoolean())  -1 else 1)
    val newPoint = Point(this.point.x + nextInt(step), this.point.y + nextInt(step))
    copy(point = newPoint)
  }

  override def energy(cfg: Configuration): Double = {
    val bondParticles = cfg.ps.filter(p => p.id == this.otherId)
    assert(bondParticles.size == 1)
    val distance = (point - bondParticles.head.point).norm(1)
    val bondEnergy = if (distance < bondLength) 0 else 1
    super.energy(cfg) + bondEnergy
  }

  override def transform(cfg: Configuration): Configuration = {
    val otherParticles = cfg.ps.filterNot(p => p.id == this.id)
    assert(otherParticles.size == cfg.ps.size - 1)
    cfg.copy(ps = otherParticles + this.float())
  }
}