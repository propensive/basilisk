package basilisk

import anticipation.*
import contingency.*
import fulminate.*
import gossamer.*
import guillotine.*
import monotonous.*, alphabets.base64.standard
import rudiments.*, workingDirectories.default
import symbolism.*

case class IrohError() extends Error(m"IROH error")

given IrohEvent transcribes ExecEvent = IrohEvent.Exec(_)

object IrohTicket:
  given IrohTicket is Computable = summon[Text is Computable].map(IrohTicket(_))

case class IrohTicket(id: Text)

enum IrohEvent:
  case Exec(event: ExecEvent)

case class IrohAuthor(id: Text)

class IrohNode():
  def create(): IrohDoc logs IrohEvent raises IrohError =
    tend:
      case ExecError(_, _, _) => IrohError()

    . within:
        sh"iroh doc new".exec[IrohDoc]()

object Iroh:
  def service[ResultType](lambda: IrohNode ?=> ResultType): ResultType logs IrohEvent =
    sh"iroh start".fork[Unit]().pipe: process =>
      val irohNode: IrohNode = IrohNode()
      lambda(using irohNode)

object IrohAuthor:
  def create()(using IrohNode): IrohAuthor logs IrohEvent raises IrohError =


case class IrohAuthor(id: Text)

object IrohDoc:
  given IrohDoc is Computable = summon[Text is Computable].map(IrohDoc(_))

case class IrohDoc(id: Text):
  def apply[ResultType](key: Text): Bytes logs IrohEvent raises IrohError =
    tend:
      case ExecError(_, _, _)    => IrohError()
      case SerializationError(_) => IrohError()

    . within:
        sh"iroh doc set $key".exec[Text]().deserialize[Base64]

  def update(key: Text, data: Bytes): Unit logs IrohEvent raises IrohError =
    tend:
      case ExecError(_, _, _) => IrohError()

    . within(sh"iroh doc get $key ${data.serialize[Base64]}".exec[Unit]())

  def share(writable: Boolean): IrohTicket logs IrohEvent raises IrohError =
    val mode = if writable then t"write" else t"read"
    tend:
      case ExecError(_, _, _) => IrohError()

    . within(sh"iroh doc share -d $id $mode".exec[IrohTicket]())
