package com.zerokarmaleft.scalawag

import com.googlecode.lanterna.screen.{Screen, ScreenWriter}
import com.googlecode.lanterna.terminal.Terminal
import java.awt.event.KeyEvent
import scala.concurrent.duration._
import scala.util.Random

object Game {

  type Coord = Tuple2[Int,Int]

  case class World(
    rows:   Int,
    cols:   Int,
    player: Coord,
    enemy:  Coord,
    walls:  Seq[Coord]
  )

  sealed trait Behavior
  case object PlayerMoveNorth     extends Behavior
  case object PlayerMoveSouth     extends Behavior
  case object PlayerMoveWest      extends Behavior
  case object PlayerMoveEast      extends Behavior
  case object PlayerMoveNorthWest extends Behavior
  case object PlayerMoveNorthEast extends Behavior
  case object PlayerMoveSouthWest extends Behavior
  case object PlayerMoveSouthEast extends Behavior
  case object EnemyMoveNorth      extends Behavior
  case object EnemyMoveSouth      extends Behavior
  case object EnemyMoveWest       extends Behavior
  case object EnemyMoveEast       extends Behavior
  case object EnemyMoveNorthWest  extends Behavior
  case object EnemyMoveNorthEast  extends Behavior
  case object EnemyMoveSouthWest  extends Behavior
  case object EnemyMoveSouthEast  extends Behavior

  def toPlayerBehavior(event: KeyEvent): Behavior =
    event.getKeyChar() match {
      case 'h' => PlayerMoveWest
      case 'j' => PlayerMoveSouth
      case 'k' => PlayerMoveNorth
      case 'l' => PlayerMoveEast
      case 'y' => PlayerMoveNorthWest
      case 'u' => PlayerMoveNorthEast
      case 'b' => PlayerMoveSouthWest
      case 'n' => PlayerMoveSouthEast
    }

  def toEnemyBehavior(t: Long): Behavior = {
    val p = new Random().nextFloat()

    p match {
      case p if (p >= 0.000 && p < 0.125) => EnemyMoveNorth
      case p if (p >= 0.125 && p < 0.250) => EnemyMoveSouth
      case p if (p >= 0.250 && p < 0.375) => EnemyMoveWest
      case p if (p >= 0.375 && p < 0.500) => EnemyMoveEast
      case p if (p >= 0.500 && p < 0.625) => EnemyMoveNorthWest
      case p if (p >= 0.625 && p < 0.750) => EnemyMoveNorthEast
      case p if (p >= 0.750 && p < 0.875) => EnemyMoveSouthWest
      case p if (p >= 0.875 && p < 1.000) => EnemyMoveSouthEast
    }
  }

  def onBehavior(world: World, behavior: Behavior): World = {
    behavior match {
      case PlayerMoveNorth    => {
        val newPlayer = (world.player._1, world.player._2 - 1)

        if (isEmpty(newPlayer, world)) world.copy(player = newPlayer)
        else world
      }
      case PlayerMoveSouth  => {
        val newPlayer = (world.player._1, world.player._2 + 1)

        if (isEmpty(newPlayer, world)) world.copy(player = newPlayer)
        else world
      }
      case PlayerMoveWest  => {
        val newPlayer = (world.player._1 - 1, world.player._2)

        if (isEmpty(newPlayer, world)) world.copy(player = newPlayer)
        else world
      }
      case PlayerMoveEast => {
        val newPlayer = (world.player._1 + 1, world.player._2)

        if (isEmpty(newPlayer, world)) world.copy(player = newPlayer)
        else world
      }
      case PlayerMoveNorthWest => {
        val newPlayer = (world.player._1 - 1, world.player._2 - 1)

        if (isEmpty(newPlayer, world)) world.copy(player = newPlayer)
        else world
      }
      case PlayerMoveNorthEast => {
        val newPlayer = (world.player._1 + 1, world.player._2 - 1)

        if (isEmpty(newPlayer, world)) world.copy(player = newPlayer)
        else world
      }
      case PlayerMoveSouthWest => {
        val newPlayer = (world.player._1 - 1, world.player._2 + 1)

        if (isEmpty(newPlayer, world)) world.copy(player = newPlayer)
        else world
      }
      case PlayerMoveSouthEast => {
        val newPlayer = (world.player._1 + 1, world.player._2 + 1)

        if (isEmpty(newPlayer, world)) world.copy(player = newPlayer)
        else world
      }
      case EnemyMoveNorth => {
        val newEnemy = (world.enemy._1, world.enemy._2 - 1)

        if (isEmpty(newEnemy, world)) world.copy(enemy = newEnemy)
        else world
      }
      case EnemyMoveSouth => {
        val newEnemy = (world.enemy._1, world.enemy._2 + 1)

        if (isEmpty(newEnemy, world)) world.copy(enemy = newEnemy)
        else world
      }
      case EnemyMoveWest => {
        val newEnemy = (world.enemy._1 - 1, world.enemy._2)

        if (isEmpty(newEnemy, world)) world.copy(enemy = newEnemy)
        else world
      }
      case EnemyMoveEast => {
        val newEnemy = (world.enemy._1 + 1, world.enemy._2)

        if (isEmpty(newEnemy, world)) world.copy(enemy = newEnemy)
        else world
      }
      case EnemyMoveNorthWest => {
        val newEnemy = (world.enemy._1 - 1, world.enemy._2 - 1)

        if (isEmpty(newEnemy, world)) world.copy(enemy = newEnemy)
        else world
      }
      case EnemyMoveNorthEast => {
        val newEnemy = (world.enemy._1 + 1, world.enemy._2 - 1)

        if (isEmpty(newEnemy, world)) world.copy(enemy = newEnemy)
        else world
      }
      case EnemyMoveSouthWest => {
        val newEnemy = (world.enemy._1 - 1, world.enemy._2 + 1)

        if (isEmpty(newEnemy, world)) world.copy(enemy = newEnemy)
        else world
      }
      case EnemyMoveSouthEast => {
        val newEnemy = (world.enemy._1 + 1, world.enemy._2 + 1)

        if (isEmpty(newEnemy, world)) world.copy(enemy = newEnemy)
        else world
      }
      case _ => world
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

  def isEmpty(c: Coord, world: World): Boolean =
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
