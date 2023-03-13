import java.awt.datatransfer.*
import java.awt.*

class ImageTransferable(private val image: Image) extends Transferable {
  def getTransferData(x: DataFlavor): Object = {
    if (isDataFlavorSupported(x)) {
      image 
    } else {
      throw new UnsupportedFlavorException(x)
    }
  }
  def isDataFlavorSupported(x: DataFlavor): Boolean = x == DataFlavor.imageFlavor
  def getTransferDataFlavors(): Array[DataFlavor] = Array(DataFlavor.imageFlavor)
}
