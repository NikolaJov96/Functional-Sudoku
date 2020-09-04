package gui

import java.awt.Color

import javax.swing.{BorderFactory, JFrame, JOptionPane}
import sudoku.GameOperations.GameFinishedState
import sudoku.{GameOperations, SudokuTable}

import scala.swing.Font.Style
import scala.swing._
import scala.swing.event.{ButtonClicked, Key, KeyPressed, MouseClicked}

class Game(val listener: GameListener) extends FlowPanel with KeyboardListener {

  val LabelWidth: Int = Application.WindowWidth
  val LabelHeight = 20
  val LabelDim = new Dimension(LabelWidth, LabelHeight)

  val FieldWidth = 30
  val FieldDim = new Dimension(FieldWidth, FieldWidth)

  val ButtonWidth = 200
  val ButtonHeight = 20
  val ButtonDim = new Dimension(ButtonWidth, ButtonHeight)

  val SelectedColor: Color = Color.CYAN.brighter().brighter()
  val NonSelectedColor: Color = Color.WHITE

  val OriginColor: Color = Color.GREEN.darker()
  val NonOriginColor: Color = Color.BLACK

  val OriginFieldSize = 15
  val OtherFieldSize = 12

  var sudoku: SudokuTable = _

  val textFields: Array[Array[TextField]] = Array.ofDim[TextField](SudokuTable.Dim, SudokuTable.Dim)
  for (i <- 0 until 9; j <- 0 until 9) textFields(i)(j) = new TextField() {
    focusable = false
    horizontalAlignment = Alignment.Center
    preferredSize = FieldDim
    border = BorderFactory.createMatteBorder(1, 1,
      i match {
        case 0 | 1 | 3 | 4 | 6 | 7 | 8 => 1
        case 2 | 5 => 2
      },
      j match {
        case 0 | 1 | 3 | 4 | 6 | 7 | 8 => 1
        case 2 | 5 => 2
      },
      Color.BLACK
    )
    listenTo(mouse.clicks)
    reactions += {
      case _: MouseClicked => table.requestFocus()
    }
  }

  val headerLabel: Label = new Label() {
    preferredSize = LabelDim
  }

  val table: GridPanel = new GridPanel(9, 9) {
    focusable = true
    for (i <- 0 until 9) {
      for (j <- 0 until 9) {
        contents += textFields(i)(j)
      }
    }
    listenTo(keys)
    reactions += { case KeyPressed(_, key, _, _) => keyPressed(key) }
  }

  contents += headerLabel

  contents += table

  contents += new Button {
    text = "Close"
    preferredSize = ButtonDim
    reactions += {
      case ButtonClicked(_) => listener.gameFinished()
    }
  }

  contents += new Button() {
    text = "Execute sequence from file"
    preferredSize = ButtonDim
    reactions += {
      case ButtonClicked(_) =>
        val fileChooser = new FileChooser()
        fileChooser.title = "Load operation sequence"
        fileChooser.showOpenDialog(null) match {
          case FileChooser.Result.Approve =>
            GameOperations.loadSequence(fileChooser.selectedFile.getAbsolutePath) match {
              case Some(seq) => GameOperations.executeSequence(sudoku, seq) match {
                case Right(newSudoku) =>
                  sudoku = newSudoku
                  syncTheTable()
                  turnEndCheck()
                case Left(error) =>
                  JOptionPane.showMessageDialog(new JFrame(),
                    error,
                    "Sequence execution error",
                    JOptionPane.WARNING_MESSAGE)
              }
              case None =>
                JOptionPane.showMessageDialog(new JFrame(),
                  "Could not load the sequence file",
                  "Loading error",
                  JOptionPane.WARNING_MESSAGE)
            }
            table.requestFocus()
          case _ => table.requestFocus()
        }
    }
  }

  contents += new Button {
    text = "Check correctness"
    preferredSize = ButtonDim
    reactions += {
      case ButtonClicked(_) =>
        GameOperations.checkGameEnd(sudoku) match {
          case GameFinishedState(true, true) =>
            JOptionPane.showMessageDialog(new JFrame(),
              "The table is completed but contains errors!",
              "Table correctness",
              JOptionPane.WARNING_MESSAGE)
          case GameFinishedState(true, false) =>
            JOptionPane.showMessageDialog(new JFrame(),
              "The game is solved!",
              "Table correctness",
              JOptionPane.INFORMATION_MESSAGE)
          case GameFinishedState(false, true) =>
            JOptionPane.showMessageDialog(new JFrame(),
              "The table is not completed, but contains errors!",
              "Table correctness",
              JOptionPane.WARNING_MESSAGE)
          case GameFinishedState(false, false) =>
            JOptionPane.showMessageDialog(new JFrame(),
              "The table is not completed, but no errors are found!",
              "Table correctness",
              JOptionPane.INFORMATION_MESSAGE)
        }
        table.requestFocus()
    }
  }

  preferredSize = Application.WindowDim

  def initGame(namedSudoku: NamedSudokuTable): Unit = {
    sudoku = namedSudoku.sudoku
    headerLabel.text = "Sudoku game: " + namedSudoku.name
    for (i <- 0 until 9; j <- 0 until 9) {
      textFields(i)(j).text = sudoku.origin(i)(j) match {
        case 0 => ""
        case x: Int => x.toString
      }
      if ((i, j) == sudoku.selectedField) textFields(i)(j).background = SelectedColor
      else textFields(i)(j).background = NonSelectedColor
      textFields(i)(j).font = sudoku.origin(i)(j) match {
        case 0 => new Font(font.getFontName, Font.Plain.id, OtherFieldSize)
        case _ => new Font(font.getFontName, Style.Bold.id, OriginFieldSize)
      }
      textFields(i)(j).foreground = sudoku.origin(i)(j) match {
        case 0 => NonOriginColor
        case _ => OriginColor
      }
    }
  }

  def frameShowed(): Unit = {
    table.requestFocus()
  }

  def syncTheTable(): Unit = {
    for (i <- 0 until 9; j <- 0 until 9) {
      textFields(i)(j).text = sudoku.table(i)(j) match {
        case 0 => ""
        case x: Int => x.toString
      }
      if ((i, j) == sudoku.selectedField) textFields(i)(j).background = SelectedColor
      else textFields(i)(j).background = NonSelectedColor
    }
  }

  def turnEndCheck(): Unit = {
    GameOperations.checkGameEnd(sudoku) match {
      case GameFinishedState(true, true) =>
        JOptionPane.showMessageDialog(
          new JFrame(), "Errors found!", "Game completed", JOptionPane.WARNING_MESSAGE)
      case GameFinishedState(true, false) =>
        JOptionPane.showMessageDialog(
          new JFrame(), "You win!", "Game completed", JOptionPane.INFORMATION_MESSAGE)
      case _ =>
    }
  }

  override def keyPressed(key: Key.Value): Unit = {
    var newNumberAdded = false
    textFields(sudoku.selectedField._1)(sudoku.selectedField._2).background = NonSelectedColor
    sudoku = key match {
      case Key.Up => GameOperations.moveSelectionUp(sudoku)
      case Key.Down => GameOperations.moveSelectionDown(sudoku)
      case Key.Left => GameOperations.moveSelectionLeft(sudoku)
      case Key.Right => GameOperations.moveSelectionRight(sudoku)
      case Key.Key1 | Key.Numpad1 | Key.Key2 | Key.Numpad2 | Key.Key3 | Key.Numpad3 |
           Key.Key4 | Key.Numpad4 | Key.Key5 | Key.Numpad5 | Key.Key6 | Key.Numpad6 |
           Key.Key7 | Key.Numpad7 | Key.Key8 | Key.Numpad8 | Key.Key9 | Key.Numpad9 =>
        val newVal = key match {
          case Key.Key1 | Key.Numpad1 => 1
          case Key.Key2 | Key.Numpad2 => 2
          case Key.Key3 | Key.Numpad3 => 3
          case Key.Key4 | Key.Numpad4 => 4
          case Key.Key5 | Key.Numpad5 => 5
          case Key.Key6 | Key.Numpad6 => 6
          case Key.Key7 | Key.Numpad7 => 7
          case Key.Key8 | Key.Numpad8 => 8
          case Key.Key9 | Key.Numpad9 => 9
        }
        GameOperations.writeTheValue(sudoku, Some(newVal)) match {
          case Left(error) =>
            JOptionPane.showMessageDialog(new JFrame(), error, "Value input error", JOptionPane.WARNING_MESSAGE)
            sudoku
          case Right(newSudoku) =>
            newNumberAdded = true
            newSudoku
        }
      case Key.Delete | Key.BackSpace => GameOperations.writeTheValue(sudoku, None) match {
        case Left(error) =>
          JOptionPane.showMessageDialog(new JFrame(), error, "Value input error", JOptionPane.WARNING_MESSAGE)
          sudoku
        case Right(newSudoku) => newSudoku
      }
      case _ => sudoku
    }
    textFields(sudoku.selectedField._1)(sudoku.selectedField._2).text =
      sudoku.table(sudoku.selectedField._1)(sudoku.selectedField._2) match {
        case 0 => ""
        case x: Int => x.toString
      }
    textFields(sudoku.selectedField._1)(sudoku.selectedField._2).background = SelectedColor
    table.requestFocus()
    if (newNumberAdded) turnEndCheck()
  }
}
