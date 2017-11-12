package lgp

import lgp.Action._
import lgp.Model.{ActionGenerators, Problem}
import lgp.crossover.{CrossoverEfectiveRandom, CrossoverHomogineous, CrossoverRandom}
import lgp.learner.{LearnerIsland, LearnerTournment}
import lgp.mutation.{MutationAddCommand, MutationDeleteCommand, MutationMicro, MutationRandomPoint}

import scala.util.Random

object HelloApp extends App {
  implicit val random = new Random()

  val inputSize = 10

  def problemBuider = Problem(
    outputSize = 1,
    constantsSize = 10,
    inputSize = inputSize,
    //    memorySize = 16,
    memorySize = 30,
    minCandidateSize = 1,
    maxCandidateSize = 100,
    numberOfCandidates = 1000,
    numberOfSteps = 1000,
    actionGenerators = ActionGenerators(Vector(
      input => PI(input.to()),
      input => V0(input.to()),
      input => Constant(input.to(), input.minValue, input.maxValue),
      input => Cos(input.to(), input.nextParam()),
      input => Sin(input.to(), input.nextParam()),
      input => Sqrt(input.to(), input.nextParam()),
      input => Log(input.to(), input.nextParam()),

      input => Addition(input.to(), input.nextParam(), input.nextParam()),
      input => Subtraction(input.to(), input.nextParam(), input.nextParam()),
      input => Multiplication(input.to(), input.nextParam(), input.nextParam()),
      input => Division(input.to(), input.nextParam(), input.nextParam()),

      input => IfBigger(input.nextParam(), input.nextParam(), input.action())
    ))
  )


  def sampleSimple: SampleRegression = {
    val parameters = (1 to inputSize map { _ => 10 * Random.nextDouble() }).toArray
    val toGuess = parameters.sum + random.nextFloat()

    SampleRegression(parameters, toGuess)
  }

  def sampleComplex: SampleRegression = {
    val parameters = (1 to inputSize map { _ => 10 * Random.nextDouble() }).toArray
    val toGuess = (
      for {
        p1 <- parameters
        p2 <- parameters
        if p1 > p2
      } yield Math.sqrt(p1 * p2)
      ).sum + random.nextFloat()

    SampleRegression(parameters, toGuess)
  }

//  def samples = for {_ <- (1 to 250).toList} yield sampleSimple
  def samples = for {_ <- (1 to 1000).toList} yield sampleComplex

  implicit val problem: Problem = problemBuider

  new Engine(
    crossovers = Vector(
      new CrossoverRandom(3),
      new CrossoverEfectiveRandom(3),
      new CrossoverHomogineous
    ),
    mutations = Vector(
      new MutationRandomPoint,
      new MutationDeleteCommand,
      new MutationMicro,
      new MutationAddCommand
    ),
    evaluator = new EvaluatorRegression,
    learner = new LearnerIsland(100, new LearnerTournment)
  ).learn(problem, samples, samples)
}
