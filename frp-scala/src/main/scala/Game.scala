package com.zerokarmaleft.scalawag

import com.googlecode.lanterna.screen.{Screen, ScreenWriter}
import com.googlecode.lanterna.terminal.Terminal
import java.awt.event.KeyEvent
import scala.concurrent.duration._
import scala.util.Random

object Game {

  type Coord = Tuple2[Int,Int]

  case class World(
    rows: Int, cols: Int,
    player: Coord,
    enemy: Coord,
    walls: Seq[Coord]
  )

  sealed trait Behavior
  case object PlayerMoveUp    extends Behavior
  case object PlayerMoveDown  extends Behavior
  case object PlayerMoveLeft  extends Behavior
  case object PlayerMoveRight extends Behavior
  case object EnemyMoveUp     extends Behavior
  case object EnemyMoveDown   extends Behavior
  case object EnemyMoveLeft   extends Behavior
  case object EnemyMoveRight  extends Behavior

  def toPlayerBehavior(event: KeyEvent): Behavior =
    event.getKeyChar() match {
      case 'w' => PlayerMoveUp
      case 'a' => PlayerMoveLeft
      case 's' => PlayerMoveDown
      case 'd' => PlayerMoveRight
    }

  def toEnemyBehavior(t: Long): Behavior = {
    val p = new Random().nextFloat()

    p match {
      case p if (p >= 0.00 && p < 0.25) => EnemyMoveUp
      case p if (p >= 0.25 && p < 0.50) => EnemyMoveDown
      case p if (p >= 0.50 && p < 0.75) => EnemyMoveLeft
      case p if (p >= 0.75 && p < 1.00) => EnemyMoveRight
    }
  }

  def onBehavior(world: World, behavior: Behavior): World = {
    behavior match {
      case PlayerMoveUp    => {
        val newPlayer = (world.player._1, world.player._2 - 1)

        if (empty(newPlayer, world)) world.copy(player = newPlayer)
        else world
      }
      case PlayerMoveDown  => {
        val newPlayer = (world.player._1, world.player._2 + 1)

        if (empty(newPlayer, world)) world.copy(player = newPlayer)
        else world
      }
      case PlayerMoveLeft  => {
        val newPlayer = (world.player._1 - 1, world.player._2)

        if (empty(newPlayer, world)) world.copy(player = newPlayer)
        else world
      }
      case PlayerMoveRight => {
        val newPlayer = (world.player._1 + 1, world.player._2)

        if (empty(newPlayer, world)) world.copy(player = newPlayer)
        else world
      }
      case EnemyMoveUp     => {
        val newEnemy = (world.enemy._1, world.enemy._2 - 1)

        if (empty(newEnemy, world)) world.copy(enemy = newEnemy)
        else world
      }
      case EnemyMoveDown   => {
        val newEnemy = (world.enemy._1, world.enemy._2 + 1)

        if (empty(newEnemy, world)) world.copy(enemy = newEnemy)
        else world
      }
      case EnemyMoveLeft   => {
        val newEnemy = (world.enemy._1 - 1, world.enemy._2)

        if (empty(newEnemy, world)) world.copy(enemy = newEnemy)
        else world
      }
      case EnemyMoveRight  => {
        val newEnemy = (world.enemy._1 + 1, world.enemy._2)

        if (empty(newEnemy, world)) world.copy(enemy = newEnemy)
        else world
      }
      case _               => world
    }
  }

  def drawWorld(world: World, screen: Screen): Unit = {
    screen.clear()

    drawFloors(world, screen)
    drawWalls(world, screen)
    drawPlayer(world.player, screen)
    drawEnemy(world.enemy, screen)

    screen.refresh()
  }

  def drawPlayer(player: Coord, screen: Screen): Unit = {
    val writer = new ScreenWriter(screen)

    writer.setForegroundColor(Terminal.Color.BLUE)
    writer.setBackgroundColor(Terminal.Color.BLACK)
    writer.drawString(player._1, player._2, "@")
  }

  def drawEnemy(enemy: Coord, screen: Screen): Unit = {
    val writer = new ScreenWriter(screen)

    writer.setForegroundColor(Terminal.Color.RED)
    writer.setBackgroundColor(Terminal.Color.BLACK)
    writer.drawString(enemy._1, enemy._2, "$")
  }

  def drawFloors(world: World, screen: Screen): Unit = {
    val writer = new ScreenWriter(screen)

    writer.setForegroundColor(Terminal.Color.GREEN)
    writer.setBackgroundColor(Terminal.Color.BLACK)

    for {
      x <- 0 until world.rows
      y <- 0 until world.cols
    } yield writer.drawString(y, x, ".")
  }

  def drawWalls(world: World, screen: Screen): Unit =
    world.walls.foreach { drawWall(_, screen)}

  def drawWall(tile: Coord, screen: Screen): Unit = {
    val writer = new ScreenWriter(screen)

    writer.setForegroundColor(Terminal.Color.WHITE)
    writer.setBackgroundColor(Terminal.Color.BLACK)
    writer.drawString(tile._1, tile._2, "#")
  }

  def empty(c: Coord, world: World): Boolean =
    !(world.player == c || world.enemy == c || world.walls.exists(_ == c))

  val gameTickDuration: Duration = 1000.millis

  def randomTiles(rows: Int, cols: Int, p: Double): Seq[Coord] = {
    val random = new Random()

    for {
      x <- 0 until rows
      y <- 0 until cols
      if random.nextFloat() < p
    } yield (y,x)
  }

}
