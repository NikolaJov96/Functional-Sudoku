package gui

import java.awt.Dimension

import scala.swing._

object Application extends SimpleSwingApplication with MenuListener with GameListener with CreatorListener {

  val WindowWidth = 400
  val WindowHeight = 440
  val WindowDim = new Dimension(WindowWidth, WindowHeight)

  val menu: Menu = new Menu(Application.this) { visible = false }
  val game: Game = new Game(Application.this) { visible = false }
  val creator: Creator = new Creator(Application.this) { visible = false }

  val mainFlowPanel: FlowPanel = new FlowPanel {
    contents += menu
    contents += game
    contents += creator
    menu.visible = true
  }

  override def top: Frame = new MainFrame {
    title = "Sudoku"
    contents = mainFlowPanel
    size = WindowDim
    resizable = false
  }

  override def startTheGame(namedSudoku: NamedSudokuTable): Unit = {
    menu.visible = false
    game.initGame(namedSudoku)
    game.visible = true
    game.frameShowed()
  }

  override def startTheCreator(namedSudoku: NamedSudokuTable): Unit ={
    menu.visible = false
    creator.initCreator(namedSudoku)
    creator.visible = true
    creator.frameShowed()
  }

  override def gameFinished(): Unit = {
    game.visible = false
    menu.visible = true
  }

  override def creatorFinished(maybeNamedSudoku: Option[NamedSudokuTable]): Unit = {
    creator.visible = false
    if (maybeNamedSudoku.isDefined) menu.addNamedSudoku(maybeNamedSudoku.get)
    menu.visible = true
  }
}
