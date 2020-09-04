package gui

import scala.swing.event.Key

trait KeyboardListener {

  def keyPressed(key: Key.Value): Unit

}
