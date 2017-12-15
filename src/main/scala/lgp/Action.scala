package lgp


import lgp.Model.ActionGeneratorsInput

import scala.util.Random

trait Action {
  def assignTo: Int

  def assignFrom: List[Int]

  def evaluate(values: Array[Double]): Unit

  def evaluateSIMD(buffer: Array[Array[Double]]): Unit

  def microMutation(actionGeneratorsInput: ActionGeneratorsInput): Action

  def cost: Int = 1
}

object Action {

  abstract class Action0(to: Int) extends Action {
    override def assignTo: Int = to

    override val assignFrom: List[Int] = Nil
  }

  case class PI(to: Int) extends Action0(to: Int) {
    override def evaluate(values: Array[Double]): Unit = values(to) = Math.PI

    override def evaluateSIMD(buffer: Array[Array[Double]]): Unit = {
      val result = buffer(to)
      var i = 0
      while (i < result.size) {
        result(i) = Math.PI
        i = i + 1
      }
    }

    override def microMutation(actionGeneratorsInput: ActionGeneratorsInput): Action = {
      PI(actionGeneratorsInput.to())
    }

    override def toString: String = s"r$to = PI"
  }

  case class E(to: Int) extends Action0(to: Int) {
    override def evaluate(values: Array[Double]): Unit = values(to) = Math.E.toFloat

    override def evaluateSIMD(buffer: Array[Array[Double]]): Unit = {
      val result = buffer(to)
      var i = 0
      while (i < result.size) {
        result(i) = Math.E
        i = i + 1
      }
    }

    override def microMutation(actionGeneratorsInput: ActionGeneratorsInput): Action = {
      E(actionGeneratorsInput.to())
    }

    override def toString: String = s"r$to = E"
  }

  case class `V+2`(to: Int) extends Action0(to: Int) {
    override def evaluate(values: Array[Double]): Unit = values(to) = 2

    override def evaluateSIMD(buffer: Array[Array[Double]]): Unit = {
      val result = buffer(to)
      var i = 0
      while (i < result.size) {
        result(i) = 2
        i = i + 1
      }
    }


    override def microMutation(actionGeneratorsInput: ActionGeneratorsInput): Action = {
      `V+2`(actionGeneratorsInput.to())
    }

    override def toString: String = s"r$to = 2"
  }

  case class `V+1`(to: Int) extends Action0(to: Int) {
    override def evaluate(values: Array[Double]): Unit = values(to) = 1

    override def evaluateSIMD(buffer: Array[Array[Double]]): Unit = {
      val result = buffer(to)
      var i = 0
      while (i < result.size) {
        result(i) = 1
        i = i + 1
      }
    }

    override def microMutation(actionGeneratorsInput: ActionGeneratorsInput): Action = {
      `V+1`(actionGeneratorsInput.to())
    }

    override def toString: String = s"r$to = 1"
  }

  case class V0(to: Int) extends Action0(to: Int) {
    override def evaluate(values: Array[Double]): Unit = values(to) = 0

    override def evaluateSIMD(buffer: Array[Array[Double]]): Unit = {
      val result = buffer(to)
      var i = 0
      while (i < result.size) {
        result(i) = 0
        i = i + 1
      }
    }

    override def microMutation(actionGeneratorsInput: ActionGeneratorsInput): Action = {
      `V0`(actionGeneratorsInput.to())
    }

    override def toString: String = s"r$to = 0"
  }

  case class `V-1`(to: Int) extends Action0(to: Int) {
    override def evaluate(values: Array[Double]): Unit = values(to) = -1

    override def evaluateSIMD(buffer: Array[Array[Double]]): Unit = {
      val result = buffer(to)
      var i = 0
      while (i < result.size) {
        result(i) = -1
        i = i + 1
      }
    }

    override def microMutation(actionGeneratorsInput: ActionGeneratorsInput): Action = {
      `V-1`(actionGeneratorsInput.to())
    }

    override def toString: String = s"r$to = -1"
  }

  case class `V-2`(to: Int) extends Action0(to: Int) {
    override def evaluate(values: Array[Double]): Unit = values(to) = -2

    override def evaluateSIMD(buffer: Array[Array[Double]]): Unit = {
      val result = buffer(to)
      var i = 0
      while (i < result.size) {
        result(i) = -2
        i = i + 1
      }
    }

    override def microMutation(actionGeneratorsInput: ActionGeneratorsInput): Action = {
      `V-2`(actionGeneratorsInput.to())
    }

    override def toString: String = s"r$to = -2"
  }

  object Constant {
    def apply(to: Int, min: Double, max: Double): Constant = {
      Constant(to, value = min + (max - min) * Random.nextFloat())
    }
  }

  case class Constant(to: Int, value: Double = Random.nextDouble()) extends Action0(to: Int) {
    override def evaluate(values: Array[Double]): Unit = values(to) = value

    override def evaluateSIMD(buffer: Array[Array[Double]]): Unit = {
      val result = buffer(to)
      var i = 0
      while (i < result.size) {
        result(i) = value
        i = i + 1
      }
    }

    override def microMutation(actionGeneratorsInput: ActionGeneratorsInput): Action = {
      Constant(actionGeneratorsInput.to(), value)
    }

    override def toString: String = s"r$to = $value"
  }

  /////////////////////

  abstract class Action1(to: Int, i1: Int) extends Action {
    override def assignTo: Int = to

    override val assignFrom = List(i1)
  }

  case class Cos(to: Int, i1: Int) extends Action1(to: Int, i1: Int) {
    override def evaluate(values: Array[Double]): Unit = values(to) = Math.cos(values(i1)).toFloat

    override def evaluateSIMD(buffer: Array[Array[Double]]): Unit = {
      val result = buffer(to)
      val source1 = buffer(i1)

      var i = 0
      while (i < result.size) {
        result(i) = Math.cos(source1(i))
        i = i + 1
      }
    }

    override def microMutation(actionGeneratorsInput: ActionGeneratorsInput): Action = {
      Random.nextInt(2) match {
        case 0 => Cos(actionGeneratorsInput.to(), i1)
        case 1 => Cos(to, actionGeneratorsInput.nextParam())
      }
    }

    override def toString: String = s"r$to = cos(r$i1)"
  }

  case class Sin(to: Int, i1: Int) extends Action1(to: Int, i1: Int) {
    override def evaluate(values: Array[Double]): Unit = values(to) = Math.sin(values(i1)).toFloat

    override def evaluateSIMD(buffer: Array[Array[Double]]): Unit = {
      val result = buffer(to)
      val source1 = buffer(i1)

      var i = 0
      while (i < result.size) {
        result(i) = Math.sin(source1(i))
        i = i + 1
      }
    }

    override def microMutation(actionGeneratorsInput: ActionGeneratorsInput): Action = {
      Random.nextInt(2) match {
        case 0 => Sin(actionGeneratorsInput.to(), i1)
        case 1 => Sin(to, actionGeneratorsInput.nextParam())
      }
    }

    override def toString: String = s"r$to = sin(r$i1)"
  }

  case class Sqrt(to: Int, i1: Int) extends Action1(to: Int, i1: Int) {
    override def evaluate(values: Array[Double]): Unit = values(to) = {
      val value = values(i1)
      val newValue = if (value > 0) Math.sqrt(value) else -Math.sqrt(-value)
      if (newValue.isNaN || newValue.isInfinity) 1000000 else newValue.toFloat
    }

    override def evaluateSIMD(buffer: Array[Array[Double]]): Unit = {
      val result = buffer(to)
      val source1 = buffer(i1)

      var i = 0
      while (i < result.size) {
        result(i) = {
          val value = source1(i1)
          val newValue = if (value > 0) Math.sqrt(value) else -Math.sqrt(-value)
          if (newValue.isNaN || newValue.isInfinity) 1000000 else newValue.toFloat
        }
        i = i + 1
      }
    }

    override def microMutation(actionGeneratorsInput: ActionGeneratorsInput): Action = {
      Random.nextInt(2) match {
        case 0 => Sqrt(actionGeneratorsInput.to(), i1)
        case 1 => Sqrt(to, actionGeneratorsInput.nextParam())
      }
    }

    override def toString: String = s"r$to = sqrt(r$i1)"
  }

  case class Log(to: Int, i1: Int) extends Action1(to: Int, i1: Int) {
    override def evaluate(values: Array[Double]): Unit = values(to) = {
      val value = values(i1)
      val newValue = if (value > 0) Math.log(value) else -Math.log(-value)
      if (newValue.isNaN || newValue.isInfinity) 1000000 else newValue.toFloat
    }

    override def evaluateSIMD(buffer: Array[Array[Double]]): Unit = {
      val result = buffer(to)
      val source1 = buffer(i1)

      var i = 0
      while (i < result.size) {
        result(i) = {
          val value = source1(i1)
          val newValue = if (value > 0) Math.log(value) else -Math.log(-value)
          if (newValue.isNaN || newValue.isInfinity) 1000000 else newValue.toFloat
        }
        i = i + 1
      }
    }

    override def microMutation(actionGeneratorsInput: ActionGeneratorsInput): Action = {
      Random.nextInt(2) match {
        case 0 => Log(actionGeneratorsInput.to(), i1)
        case 1 => Log(to, actionGeneratorsInput.nextParam())
      }
    }

    override def toString: String = s"r$to = log(r$i1)"

  }

  /////////////////////

  abstract class Action2(to: Int, i1: Int, i2: Int) extends Action {
    override def assignTo: Int = to

    override val assignFrom = List(i1, i2)
  }

  case class Addition(to: Int, i1: Int, i2: Int) extends Action2(to, i1: Int, i2: Int) {
    override def evaluate(values: Array[Double]): Unit = {
      values(to) = values(i1) + values(i2)
    }

    override def evaluateSIMD(buffer: Array[Array[Double]]): Unit = {
      val result = buffer(to)
      val source1 = buffer(i1)
      val source2 = buffer(i2)

      var i = 0
      while (i < result.size) {
        result(i) = source1(i) + source2(i)
        i = i + 1
      }
    }

    override def microMutation(actionGeneratorsInput: ActionGeneratorsInput): Action = {
      Random.nextInt(3) match {
        case 0 => Addition(actionGeneratorsInput.to(), i1, i2)
        case 1 => Addition(to, actionGeneratorsInput.nextParam(), i2)
        case 2 => Addition(to, i1, actionGeneratorsInput.nextParam())
      }
    }

    override def toString: String = s"r$to = r$i1 + r$i2"
  }

  case class Subtraction(to: Int, i1: Int, i2: Int) extends Action2(to, i1: Int, i2: Int) {
    override def evaluate(values: Array[Double]): Unit = {
      values(to) = values(i1) - values(i2)
    }

    override def evaluateSIMD(buffer: Array[Array[Double]]): Unit = {
      val result = buffer(to)
      val source1 = buffer(i1)
      val source2 = buffer(i2)

      var i = 0
      while (i < result.size) {
        result(i) = source1(i) - source2(i)
        i = i + 1
      }
    }

    override def microMutation(actionGeneratorsInput: ActionGeneratorsInput): Action = {
      Random.nextInt(3) match {
        case 0 => Subtraction(actionGeneratorsInput.to(), i1, i2)
        case 1 => Subtraction(to, actionGeneratorsInput.nextParam(), i2)
        case 2 => Subtraction(to, i1, actionGeneratorsInput.nextParam())
      }
    }

    override def toString: String = s"r$to = r$i1 - r$i2"
  }

  case class Multiplication(to: Int, i1: Int, i2: Int) extends Action2(to, i1: Int, i2: Int) {
    override def evaluate(values: Array[Double]): Unit = {
      values(to) = values(i1) * values(i2)
    }

    override def evaluateSIMD(buffer: Array[Array[Double]]): Unit = {
      val result = buffer(to)
      val source1 = buffer(i1)
      val source2 = buffer(i2)

      var i = 0
      while (i < result.size) {
        result(i) = source1(i) * source2(i)
        i = i + 1
      }
    }


    override def microMutation(actionGeneratorsInput: ActionGeneratorsInput): Action = {
      Random.nextInt(3) match {
        case 0 => Multiplication(actionGeneratorsInput.to(), i1, i2)
        case 1 => Multiplication(to, actionGeneratorsInput.nextParam(), i2)
        case 2 => Multiplication(to, i1, actionGeneratorsInput.nextParam())
      }
    }

    override def toString: String = s"r$to = r$i1 * r$i2"
  }

  case class Division(to: Int, i1: Int, i2: Int) extends Action2(to, i1: Int, i2: Int) {
    override def evaluate(values: Array[Double]): Unit = values(to) = {
      val newValue = values(i1) / values(i2)
      if (newValue.isNaN || newValue.isInfinity) 1000000 else newValue.toFloat
    }

    override def evaluateSIMD(buffer: Array[Array[Double]]): Unit = {
      val result = buffer(to)
      val source1 = buffer(i1)
      val source2 = buffer(i2)

      var i = 0
      while (i < result.size) {
        result(i) = {
          val newValue = source1(i) / source2(i)
          if (newValue.isNaN || newValue.isInfinity) 1000000 else newValue.toFloat
        }
        i = i + 1
      }
    }

    override def microMutation(actionGeneratorsInput: ActionGeneratorsInput): Action = {
      Random.nextInt(3) match {
        case 0 => Division(actionGeneratorsInput.to(), i1, i2)
        case 1 => Division(to, actionGeneratorsInput.nextParam(), i2)
        case 2 => Division(to, i1, actionGeneratorsInput.nextParam())
      }
    }

    override def toString: String = s"r$to = r$i1 / r$i2"
  }

  case class IfBigger(i1: Int, i2: Int, action: Action) extends Action {
    override def assignTo: Int = action.assignTo

    override def assignFrom: List[Int] = action.assignTo :: i1 :: i2 :: action.assignFrom

    override def evaluate(values: Array[Double]): Unit = {
      if (values(i1) > values(i2)) {
        action.evaluate(values)
      }
    }

    var newColumn: Array[Double] = null

    override def evaluateSIMD(buffer: Array[Array[Double]]): Unit = {
      val outIndex = assignTo
      val oldColumn = buffer(outIndex)
      val newColumn = oldColumn.clone()

      buffer(outIndex) = newColumn

      action.evaluateSIMD(buffer)

      buffer(outIndex) = oldColumn

      val source1 = buffer(i1)
      val source2 = buffer(i2)

      var i = 0
      while (i < oldColumn.size) {
        if (source1(i) > source2(i)) {
          oldColumn(i) = newColumn(i)
        }
        i = i + 1
      }
    }

    override def microMutation(actionGeneratorsInput: ActionGeneratorsInput): Action = {
      Random.nextInt(4) match {
        case 0 => IfBigger(actionGeneratorsInput.nextParam(), i1, action)
        case 1 => IfBigger(i1, actionGeneratorsInput.nextParam(), action)
        case 2 => IfBigger(i1, i2, action.microMutation(actionGeneratorsInput))
        case 3 => action
      }
    }

    override def cost: Int = 10

    override def toString: String = s"if r$i1 > r$i2 then $action"
  }

}