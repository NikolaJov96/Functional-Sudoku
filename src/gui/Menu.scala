package gui

import javax.swing.{JFrame, JOptionPane}
import sudoku.{GameOperations, Solver, SudokuTable}

import scala.collection.mutable
import scala.swing.ListView.Renderer
import scala.swing.event.{ButtonClicked, Key}
import scala.swing._

class Menu(val listener: MenuListener) extends FlowPanel with KeyboardListener {

  val ElemWidth = 200
  val ButtonHeight = 20
  val ListHeight = 200
  val ButtonDim = new Dimension(ElemWidth, ButtonHeight)
  val ListDim = new Dimension(ElemWidth, ListHeight)

  val sudokuTables: mutable.Buffer[NamedSudokuTable] = mutable.Buffer()

  val listView: ListView[NamedSudokuTable] = new ListView[NamedSudokuTable]() {
    listData = sudokuTables
    renderer = Renderer(_.name)
  }

  def addNamedSudoku(namedSudoku: NamedSudokuTable): Unit = {
    sudokuTables += namedSudoku
    listView.listData = sudokuTables
  }

  contents += new Button {
    text = "Load from file"
    preferredSize = ButtonDim
    reactions += {
      case ButtonClicked(_) =>
        val fileChooser = new FileChooser()
        fileChooser.title = "Load sudoku game"
        fileChooser.showOpenDialog(null) match {
          case FileChooser.Result.Approve =>
            SudokuTable.createFromFile(fileChooser.selectedFile.getAbsolutePath) match {
              case Right(sudoku) => addNamedSudoku(NamedSudokuTable(fileChooser.selectedFile.getName, sudoku))
              case Left(error) =>
                JOptionPane.showMessageDialog(new JFrame(), error, "Loading error", JOptionPane.WARNING_MESSAGE)
            }
          case _ =>
        }
    }
  }

  contents += new ScrollPane(listView) {
    preferredSize = ListDim
  }

  contents += new Button() {
    text = "Play"
    preferredSize = ButtonDim
    reactions += {
      case ButtonClicked(_) =>
        if (listView.selection.items.isEmpty || listView.selection.items.length > 1)
          JOptionPane.showMessageDialog(
            new JFrame(),
            "Please select one of the loaded games",
            "Cannot start the game",
            JOptionPane.WARNING_MESSAGE)
        else listener.startTheGame(listView.selection.items.head)
    }
  }

  contents += new Button {
    text = "Modify"
    preferredSize = ButtonDim
    reactions += {
      case ButtonClicked(_) =>
        if (listView.selection.items.isEmpty || listView.selection.items.length > 1)
          JOptionPane.showMessageDialog(
            new JFrame(),
            "Please select one of the loaded games",
            "Cannot start the game creator",
            JOptionPane.WARNING_MESSAGE)
        else listener.startTheCreator(listView.selection.items.head)
    }
  }

  contents += new Button {
    text = "Generate solution"
    preferredSize = ButtonDim
    reactions += {
      case ButtonClicked(_) =>
        if (listView.selection.items.isEmpty || listView.selection.items.length > 1)
          JOptionPane.showMessageDialog(
            new JFrame(),
            "Please select one of the loaded games",
            "Cannot start the game creator",
            JOptionPane.WARNING_MESSAGE)
        else {
          Solver.solve(listView.selection.items.head.sudoku) match {
            case Left(error) =>
              JOptionPane.showMessageDialog(
                new JFrame(),
                error,
                "Solving unsuccessful",
                JOptionPane.WARNING_MESSAGE)
            case Right(result) =>
              val fileChooser = new FileChooser()
              fileChooser.title = "Save solution sequence"
              fileChooser.showSaveDialog(null) match {
                case FileChooser.Result.Approve =>
                  GameOperations.saveSequence(fileChooser.selectedFile.getAbsolutePath, result._2)
                case _ =>
              }
          }
        }
    }
  }

  preferredSize = Application.WindowDim

  override def keyPressed(key: Key.Value): Unit = {}

}
