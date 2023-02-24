package net.bulbyvr.swing

import scala.swing.ComboBox
class ComboModel[A] extends javax.swing.DefaultComboBoxModel[A] {
  def +=(elem: A) =  addElement(elem)
  def ++=(elems: TraversableOnce[A]) =  elems.foreach(addElement) 
}

class MutableComboBox[A](seq : Seq[A]) extends ComboBox[A](seq) {
  private val model = ComboModel[A]()
  peer.setModel(model)
  object items {
    def ++=(elems : TraversableOnce[A]) = model ++= elems 
    def +=(elem : A) = model += elem 
    def removeAllElements() = model.removeAllElements()
  }
}

