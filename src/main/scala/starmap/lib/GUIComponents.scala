package starmap.lib

import javax.swing.{ButtonGroup, ButtonModel}

private[lib] object GUIComponents {

  // Les lignes décrites dans le menu déroulant
  private[lib] case class LineItem(id: String, num: String, lib: String) {
    override def toString(): String = num + " - " + lib
  }

  // Extension simple de ButtonGroup pour autoriser aucun bouton sélectionné.
  private[lib] class NoneSelectedButtonGroup extends ButtonGroup {
    override def setSelected(model: ButtonModel, selected: Boolean): Unit = {
      if (selected) { super.setSelected(model, selected) }
      else { clearSelection() }
    }
  }

  private[lib] sealed trait ClickType
  private[lib] case object AStartClick extends ClickType {
    override def toString(): String = "Départ"
  }
  private[lib] case object BEndClick extends ClickType {
    override def toString(): String = "Arrivée"
  }
  private[lib] case object ZCancelClick extends ClickType {
    override def toString(): String = "Annuler"
  }

}