package provingground.nlp

import scala.collection.AbstractIterator

/**
  * This is a class developed to iterate over all possible values of a boolean array,
  * with the with the lowest number of trues earlier.
  * The iterator starts at true, false, false ...
  * NOTE: The current implementation is quite messy, and should be cleaned up.
  */
class BinaryArray(length: Int) extends Iterable[Array[Boolean]] {
  var array: Array[Boolean] = (for (_ <- 0 until length) yield false).toArray

  override def iterator: Iterator[Array[Boolean]] = {
    new BinaryArrayIterator(this)
  }
}
class BinaryArrayIterator(binaryArray: BinaryArray)
    extends AbstractIterator[Array[Boolean]] {

  private var numberOfTrue: Int = 0

  override def hasNext: Boolean = {
    numberOfTrue != binaryArray.array.length
  }

  override def next(): Array[Boolean] = {
    var i          = binaryArray.array.length - 1
    var updateDone = false
    while (i > 0 && !updateDone) {
      if (!binaryArray.array(i) && binaryArray.array(i - 1)) {
        updateDone = true
        binaryArray.array.update(i, true)
        binaryArray.array.update(i - 1, false)
      }
      i -= 1
    }
    if (!updateDone) {
      numberOfTrue += 1
      for (i <- binaryArray.array.indices) {
        binaryArray.array.update(i, i < numberOfTrue)
      }
    }
    binaryArray.array
  }
}
