package gui

import java.awt.Color

import javax.swing.{BorderFactory, JFrame, JOptionPane}
import sudoku._

import scala.collection.mutable.ListBuffer
import scala.swing.ListView.Renderer
import scala.swing._
import scala.swing.event.{ButtonClicked, Key, KeyPressed, MouseClicked}

class Creator(val listener: CreatorListener) extends FlowPanel with KeyboardListener {

  case class NamedSequence(name: String, opList: CreatorOperation)

  val LabelWidth: Int = Application.WindowWidth
  val LabelHeight = 10
  val LabelDim = new Dimension(LabelWidth, LabelHeight)

  val ListPanelWidth = 130
  val ListPanelHeight = 230
  val ListPanelDim = new Dimension(ListPanelWidth, ListPanelHeight)
  val ListHeight = 90
  val ListDim = new Dimension(ListPanelWidth, ListHeight)

  val FieldWidth = 24
  val FieldDim = new Dimension(FieldWidth, FieldWidth)

  val ButtonWidth = 150
  val ButtonHeight = 20
  val ButtonDim = new Dimension(ButtonWidth, ButtonHeight)
  val ListButtonWidth = 120
  val ListButtonDim = new Dimension(ListButtonWidth, ButtonHeight)

  val SelectedColor: Color = Color.RED.brighter().brighter()
  val EditingColor: Color = Color.CYAN.brighter().brighter()
  val NonSelectedColor: Color = Color.WHITE

  var sudoku: SudokuTable = _
  var editing: (Int, Int) = _
  var namedOperations: ListBuffer[NamedSequence] = ListBuffer[NamedSequence]()
  var currOperations: CreatorOperation = NilOperation

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

  val headerLabel: Label = new Label() { preferredSize = LabelDim }

  val sequenceListView: ListView[NamedSequence] = new ListView[NamedSequence]() {
    listData = namedOperations
    renderer = Renderer(_.name)
  }

  val operationListView: ListView[CreatorOperation] = new ListView[CreatorOperation]() {
    listData = currOperations
    renderer = Renderer(_.toString())
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

  contents += new FlowPanel() {
    preferredSize = ListPanelDim
    contents += new Label("Sequences:") { preferredSize = LabelDim }
    contents += new ScrollPane(sequenceListView) { preferredSize = ListDim }
    contents += new Label("Operations:") { preferredSize = LabelDim }
    contents += new ScrollPane(operationListView) { preferredSize = ListDim }
  }

  contents += table

  contents += new Button() {
    text = "Set starting position"
    preferredSize = ButtonDim
    reactions += {
      case ButtonClicked(_) =>
        GameCreator.setStartingField(sudoku, editing._1, editing._2) match {
          case Left(error) =>
            JOptionPane.showMessageDialog(new JFrame(), error, "Changing the staring position", JOptionPane.WARNING_MESSAGE)
          case Right(mewSudoku) =>
            textFields(sudoku.selectedField._1)(sudoku.selectedField._2).background = NonSelectedColor
            sudoku = mewSudoku
            textFields(sudoku.selectedField._1)(sudoku.selectedField._2).background = EditingColor
            addOperation(new SetStartingFieldOp(editing._1, editing._2, NilOperation))
        }
        table.requestFocus()
    }
  }

  contents += new Button() {
    text = "Check for errors"
    preferredSize = ButtonDim
    reactions += {
      case ButtonClicked(_) =>
        Solver.solve(sudoku) match {
          case Left(error) =>
            JOptionPane.showMessageDialog(new JFrame(), error, "Checking if solvable", JOptionPane.WARNING_MESSAGE)
          case Right(_) =>
            JOptionPane.showMessageDialog(
              new JFrame(),
              "The game is solvable!",
              "Checking if solvable",
              JOptionPane.INFORMATION_MESSAGE)
        }
        table.requestFocus()
    }
  }

  contents += new Button() {
    text = "Transpose"
    preferredSize = ButtonDim
    reactions += {
      case ButtonClicked(_) =>
        sudoku = GameCreator.transpose(sudoku)
        syncTheTable()
        addOperation(new TransposeOp(NilOperation))
        table.requestFocus()
    }
  }

  contents += new Button() {
    text = "Invert numbers"
    preferredSize = ButtonDim
    reactions += {
      case ButtonClicked(_) =>
        sudoku = GameCreator.invertNumbers(sudoku)
        syncTheTable()
        addOperation(new InvertNumbersOp(NilOperation))
        table.requestFocus()
    }
  }

  contents += new Button() {
    text = "Filter lines"
    preferredSize = ButtonDim
    reactions += {
      case ButtonClicked(_) =>
        GameCreator.filterLines(sudoku, editing._1, editing._2) match {
          case Left(error) =>
            JOptionPane.showMessageDialog(new JFrame(), error, "Filtering", JOptionPane.WARNING_MESSAGE)
          case Right(newSudoku) =>
            sudoku = newSudoku
            syncTheTable()
            addOperation(new FilterLinesOp(editing._1, editing._2, NilOperation))
        }
        table.requestFocus()
    }
  }

  contents += new Button() {
    text = "Filter boxes"
    preferredSize = ButtonDim
    reactions += {
      case ButtonClicked(_) =>
        GameCreator.filterBox(sudoku, editing._1, editing._2) match {
          case Left(error) =>
            JOptionPane.showMessageDialog(new JFrame(), error, "Filtering", JOptionPane.WARNING_MESSAGE)
          case Right(newSudoku) =>
            sudoku = newSudoku
            syncTheTable()
            addOperation(new FilterBoxOp(editing._1, editing._2, NilOperation))
        }
        table.requestFocus()
    }
  }

  contents += new Button {
    text = "Reset seq"
    preferredSize = ListButtonDim
    reactions += {
      case ButtonClicked(_) =>
        currOperations = NilOperation
        operationListView.listData = currOperations
        table.requestFocus()
    }
  }

  contents += new Button {
    text = "Save seq"
    preferredSize = ListButtonDim
    reactions += {
      case ButtonClicked(_) =>
        if (currOperations != NilOperation)
          JOptionPane.showInputDialog(new JFrame(), "Input the name of the sequence:") match {
            case name: String if name.length > 0 =>
              if (namedOperations.exists({ op => op.name == name }))
                JOptionPane.showMessageDialog(
                  new JFrame(),
                  "Sequence name already exists!",
                  "Saving the sequence",
                  JOptionPane.WARNING_MESSAGE)
              else {
                namedOperations += NamedSequence(name, currOperations)
                currOperations = NilOperation
                sequenceListView.listData = namedOperations
                operationListView.listData = currOperations
              }
            case _ => JOptionPane.showMessageDialog(
              new JFrame(),
              "Invalid sequence name!",
              "Saving the sequence",
              JOptionPane.WARNING_MESSAGE)
          }
        else JOptionPane.showMessageDialog(
          new JFrame(),
          "The sequence is empty!",
          "Saving the sequence",
          JOptionPane.WARNING_MESSAGE)
    }
  }

  contents += new Button {
    text = "Apply seq"
    preferredSize = ListButtonDim
    reactions += {
      case ButtonClicked(_) =>
        if (sequenceListView.selection.items.isEmpty || sequenceListView.selection.items.length > 1)
          JOptionPane.showMessageDialog(
            new JFrame(),
            "Problem with named sequence selection",
            "Applying the named sequence",
            JOptionPane.WARNING_MESSAGE)
        else {
          val seqName = sequenceListView.selection.items.head.name
          val seqHead = sequenceListView.selection.items.head.opList
          seqHead.execute(sudoku) match {
            case Left(error) => JOptionPane.showMessageDialog(
              new JFrame(),
              error,
              "Applying the named sequence",
              JOptionPane.WARNING_MESSAGE)
            case Right(newSudoku) =>
              sudoku = newSudoku
              syncTheTable()
              addOperation(new CompositeOperationOp(seqHead, seqName, NilOperation))
          }
        }
        table.requestFocus()
    }
  }

  contents += new Button {
    text = "Close"
    preferredSize = ButtonDim
    reactions += {
      case ButtonClicked(_) =>
        JOptionPane.showConfirmDialog(
          new JFrame(),
          "Are you sure that you want to discard the new sudoku table?",
          "Exiting creator",
          JOptionPane.YES_NO_OPTION) match {
          case JOptionPane.YES_OPTION => listener.creatorFinished(None)
          case _ =>
        }
        table.requestFocus()
    }
  }

  contents += new Button {
    text = "Save table"
    preferredSize = ButtonDim
    reactions += {
      case ButtonClicked(_) =>
        val fileChooser = new FileChooser()
        fileChooser.title = "Save created sudoku table"
        fileChooser.showSaveDialog(null) match {
          case FileChooser.Result.Approve =>
            if (sudoku.saveToFile(fileChooser.selectedFile.getAbsolutePath))
              listener.creatorFinished(Some(NamedSudokuTable(fileChooser.selectedFile.getName, sudoku)))
            else JOptionPane.showMessageDialog(new JFrame(),
              "Could not save sudoku table to the file!",
              "Saving the sudoku table",
              JOptionPane.WARNING_MESSAGE)
            table.requestFocus()
          case _ => table.requestFocus()
        }
        table.requestFocus()
    }
  }

  preferredSize = Application.WindowDim

  def initCreator(namedSudoku: NamedSudokuTable): Unit = {
    sudoku = namedSudoku.sudoku
    headerLabel.text = "Editing original game: " + namedSudoku.name
    editing = sudoku.selectedField
    currOperations = NilOperation
    operationListView.listData = currOperations
    syncTheTable()
  }

  def frameShowed(): Unit = {
    table.requestFocus()
  }

  def addOperation(op: CreatorOperation): Unit = {
    currOperations = currOperations.insert(op)
    operationListView.listData = currOperations
  }

  def syncTheTable(): Unit = {
    for (i <- 0 until 9; j <- 0 until 9) {
      textFields(i)(j).text = sudoku.origin(i)(j) match {
        case 0 => ""
        case x: Int => x.toString
      }
      if ((i, j) == editing) textFields(i)(j).background = EditingColor
      else if ((i, j) == sudoku.selectedField) textFields(i)(j).background = SelectedColor
      else textFields(i)(j).background = NonSelectedColor
    }
    table.requestFocus()
  }

  override def keyPressed(key: Key.Value): Unit = {
    textFields(sudoku.selectedField._1)(sudoku.selectedField._2).background = NonSelectedColor
    textFields(editing._1)(editing._2).background = NonSelectedColor
    key match {
      case Key.Up => if (editing._1 > 0) editing = (editing._1 - 1, editing._2)
      case Key.Down => if (editing._1 < 8) editing = (editing._1 + 1, editing._2)
      case Key.Left => if (editing._2 > 0) editing = (editing._1, editing._2 - 1)
      case Key.Right => if (editing._2 < 8) editing = (editing._1, editing._2 + 1)
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
        if (editing != sudoku.selectedField)
          GameCreator.setField(sudoku, editing._1, editing._2, Some(newVal)) match {
            case Left(error) =>
              JOptionPane.showMessageDialog(new JFrame(), error, "Value input error", JOptionPane.WARNING_MESSAGE)
            case Right(newSudoku) =>
              sudoku = newSudoku
              addOperation(new SetFieldOp(editing._1, editing._2, Some(newVal), NilOperation))
          }
        else JOptionPane.showMessageDialog(
          new JFrame(),
          "Can not write into the staring field",
          "Value input error",
          JOptionPane.WARNING_MESSAGE)
      case Key.Delete | Key.BackSpace =>
        GameCreator.setField(sudoku, editing._1, editing._2, None) match {
          case Left(error) =>
            JOptionPane.showMessageDialog(new JFrame(), error, "Value input error", JOptionPane.WARNING_MESSAGE)
          case Right(newSudoku) =>
            sudoku = newSudoku
            addOperation(new SetFieldOp(editing._1, editing._2, None, NilOperation))
        }
      case _ =>
    }
    textFields(editing._1)(editing._2).text =
      sudoku.origin(editing._1)(editing._2) match {
        case 0 => ""
        case x: Int => x.toString
      }
    textFields(sudoku.selectedField._1)(sudoku.selectedField._2).background = SelectedColor
    textFields(editing._1)(editing._2).background = EditingColor
    table.requestFocus()
  }
}
