package com.zerokarmaleft.scalawag

import com.googlecode.lanterna.screen.{Screen}
import com.googlecode.lanterna.terminal.{Terminal, TerminalSize}
import com.googlecode.lanterna.terminal.swing.SwingTerminal
import java.awt.event.KeyEvent
import javax.swing.JFrame
import rx.lang.scala._
import rx.observables.SwingObservable

object Main {

  import Game._
  import rx.lang.scala.JavaConversions._

  def main(args: Array[String]): Unit = {
    val terminal     = new SwingTerminal(new TerminalSize(160, 50))
    terminal.setCursorVisible(false)
    val screen       = new Screen(terminal)
    val terminalSize = terminal.getTerminalSize()
    val rows         = terminalSize.getRows()
    val cols         = terminalSize.getColumns()

    screen.startScreen()
    val frame = terminal.getJFrame()
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)

    val playerBehaviors = SwingObservable.
      fromKeyEvents(frame).
      filter { event: KeyEvent => event.getID == KeyEvent.KEY_TYPED }.
      map { toPlayerBehavior }

    val enemyBehaviors = Observable.
      interval(gameTickDuration).
      map { toEnemyBehavior }

    val initWorld = World(rows, cols, (0,0), (25,25), randomTiles(rows, cols, 0.25))

    val worlds = playerBehaviors.merge(enemyBehaviors).
      scan(initWorld) { (oldWorld, behavior) => onBehavior(oldWorld, behavior) }.
      subscribe { drawWorld(_, screen) }
  }

}
