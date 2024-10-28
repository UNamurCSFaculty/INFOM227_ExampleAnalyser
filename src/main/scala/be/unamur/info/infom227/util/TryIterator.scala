package be.unamur.info.infom227.util

import scala.util.{Success, Try}

object TryIterator {
  implicit class TryIteratorOps[T](self: Iterator[Try[T]]) {
    def toTry: Try[Iterator[T]] = {
      self.nextOption match
        case Some(next) =>
          for {
            head <- next
            tail <- self.toTry
          } yield Iterator(head) ++ tail
        case None => Success(Iterator.empty)
    }
  }
}
