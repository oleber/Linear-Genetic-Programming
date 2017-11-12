package lgp


import lgp.Model.ActionGeneratorsInput

import scala.util.Random

trait Action {
  def assignTo: Int

  def assignFrom: List[Int]

  def evaluate(values: Array[Double]): Unit

  def microMutation(actionGeneratorsInput: ActionGeneratorsInput): Action
}

object Action {

  abstract class Action0(to: Int) extends Action {
    override def assignTo: Int = to

    override val assignFrom: List[Int] = Nil
  }

  case class PI(to: Int) extends Action0(to: Int) {
    override def evaluate(values: Array[Double]): Unit = values(to) = Math.PI.toFloat

    override def microMutation(actionGeneratorsInput: ActionGeneratorsInput): Action = {
      PI(actionGeneratorsInput.to())
    }
  }

  case class E(to: Int) extends Action0(to: Int) {
    override def evaluate(values: Array[Double]): Unit = values(to) = Math.E.toFloat

    override def microMutation(actionGeneratorsInput: ActionGeneratorsInput): Action = {
      E(actionGeneratorsInput.to())
    }
  }

  case class `V+2`(to: Int) extends Action0(to: Int) {
    override def evaluate(values: Array[Double]): Unit = values(to) = 2

    override def microMutation(actionGeneratorsInput: ActionGeneratorsInput): Action = {
      `V+2`(actionGeneratorsInput.to())
    }
  }

  case class `V+1`(to: Int) extends Action0(to: Int) {
    override def evaluate(values: Array[Double]): Unit = values(to) = 1
    override def microMutation(actionGeneratorsInput: ActionGeneratorsInput): Action = {
      `V+1`(actionGeneratorsInput.to())
    }
  }

  case class V0(to: Int) extends Action0(to: Int) {
    override def evaluate(values: Array[Double]): Unit = values(to) = 0
    override def microMutation(actionGeneratorsInput: ActionGeneratorsInput): Action = {
      `V0`(actionGeneratorsInput.to())
    }
  }

  case class `V-1`(to: Int) extends Action0(to: Int) {
    override def evaluate(values: Array[Double]): Unit = values(to) = -1
    override def microMutation(actionGeneratorsInput: ActionGeneratorsInput): Action = {
      `V-1`(actionGeneratorsInput.to())
    }
  }

  case class `V-2`(to: Int) extends Action0(to: Int) {
    override def evaluate(values: Array[Double]): Unit = values(to) = -2
    override def microMutation(actionGeneratorsInput: ActionGeneratorsInput): Action = {
      `V-2`(actionGeneratorsInput.to())
    }
  }

  object Constant {
    def apply(to: Int, min: Double, max: Double): Constant = {
      Constant(to, value = min + (max - min) * Random.nextFloat())
    }
  }

  case class Constant(to: Int, value: Double = Random.nextDouble()) extends Action0(to: Int) {
    override def evaluate(values: Array[Double]): Unit = values(to) = value
    override def microMutation(actionGeneratorsInput: ActionGeneratorsInput): Action = {
      Constant(actionGeneratorsInput.to(), value)
    }
  }

  /////////////////////

  abstract class Action1(to: Int, i1: Int) extends Action {
    override def assignTo: Int = to

    override val assignFrom = List(i1)
  }

  case class Cos(to: Int, i1: Int) extends Action1(to: Int, i1: Int) {
    override def evaluate(values: Array[Double]): Unit = values(to) = Math.cos(values(i1)).toFloat

    override def microMutation(actionGeneratorsInput: ActionGeneratorsInput): Action = {
      Random.nextInt(2) match {
        case 0 => Cos(actionGeneratorsInput.to(), i1)
        case 1 => Cos(to, actionGeneratorsInput.nextParam())
      }
    }
  }

  case class Sin(to: Int, i1: Int) extends Action1(to: Int, i1: Int) {
    override def evaluate(values: Array[Double]): Unit = values(to) = Math.sin(values(i1)).toFloat

    override def microMutation(actionGeneratorsInput: ActionGeneratorsInput): Action = {
      Random.nextInt(2) match {
        case 0 => Sin(actionGeneratorsInput.to(), i1)
        case 1 => Sin(to, actionGeneratorsInput.nextParam())
      }
    }
  }

  case class Sqrt(to: Int, i1: Int) extends Action1(to: Int, i1: Int) {
    override def evaluate(values: Array[Double]): Unit = values(to) = {
      val value = values(i1)
      val newValue = if (value > 0) Math.sqrt(value) else -Math.sqrt(-value)
      if (newValue.isNaN || newValue.isInfinity) 1000000 else newValue.toFloat
    }

    override def microMutation(actionGeneratorsInput: ActionGeneratorsInput): Action = {
      Random.nextInt(2) match {
        case 0 => Sqrt(actionGeneratorsInput.to(), i1)
        case 1 => Sqrt(to, actionGeneratorsInput.nextParam())
      }
    }
  }

  case class Log(to: Int, i1: Int) extends Action1(to: Int, i1: Int) {
    override def evaluate(values: Array[Double]): Unit = values(to) = {
      val value = values(i1)
      val newValue = if (value > 0) Math.log(value) else -Math.log(-value)
      if (newValue.isNaN || newValue.isInfinity) 1000000 else newValue.toFloat
    }

    override def microMutation(actionGeneratorsInput: ActionGeneratorsInput): Action = {
      Random.nextInt(2) match {
        case 0 => Log(actionGeneratorsInput.to(), i1)
        case 1 => Log(to, actionGeneratorsInput.nextParam())
      }
    }
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

    override def microMutation(actionGeneratorsInput: ActionGeneratorsInput): Action = {
      Random.nextInt(3) match {
        case 0 => Addition(actionGeneratorsInput.to(), i1, i2)
        case 1 => Addition(to, actionGeneratorsInput.nextParam(), i2)
        case 2 => Addition(to, i1, actionGeneratorsInput.nextParam())
      }
    }
  }

  case class Subtraction(to: Int, i1: Int, i2: Int) extends Action2(to, i1: Int, i2: Int) {
    override def evaluate(values: Array[Double]): Unit = {
      values(to) = values(i1) - values(i2)
    }
    override def microMutation(actionGeneratorsInput: ActionGeneratorsInput): Action = {
      Random.nextInt(3) match {
        case 0 => Subtraction(actionGeneratorsInput.to(), i1, i2)
        case 1 => Subtraction(to, actionGeneratorsInput.nextParam(), i2)
        case 2 => Subtraction(to, i1, actionGeneratorsInput.nextParam())
      }
    }
  }

  case class Multiplication(to: Int, i1: Int, i2: Int) extends Action2(to, i1: Int, i2: Int) {
    override def evaluate(values: Array[Double]): Unit = {
      values(to) = values(i1) * values(i2)
    }

    override def microMutation(actionGeneratorsInput: ActionGeneratorsInput): Action = {
      Random.nextInt(3) match {
        case 0 => Multiplication(actionGeneratorsInput.to(), i1, i2)
        case 1 => Multiplication(to, actionGeneratorsInput.nextParam(), i2)
        case 2 => Multiplication(to, i1, actionGeneratorsInput.nextParam())
      }
    }
  }

  case class Division(to: Int, i1: Int, i2: Int) extends Action2(to, i1: Int, i2: Int) {
    override def evaluate(values: Array[Double]): Unit = values(to) = {
      val newValue = values(i1) / values(i2)
      if (newValue.isNaN || newValue.isInfinity) 1000000 else newValue.toFloat
    }

    override def microMutation(actionGeneratorsInput: ActionGeneratorsInput): Action = {
      Random.nextInt(3) match {
        case 0 => Division(actionGeneratorsInput.to(), i1, i2)
        case 1 => Division(to, actionGeneratorsInput.nextParam(), i2)
        case 2 => Division(to, i1, actionGeneratorsInput.nextParam())
      }
    }
  }

  case class IfBigger(i1: Int, i2: Int, action: Action) extends Action {
    override def assignTo: Int = action.assignTo

    override def assignFrom: List[Int] = action.assignTo :: i1 :: i2 :: action.assignFrom

    override def evaluate(values: Array[Double]): Unit = {
      if (values(i1) > values(i2)) {
        action.evaluate(values)
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
  }

}