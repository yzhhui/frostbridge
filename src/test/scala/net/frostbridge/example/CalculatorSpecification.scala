/*
*  Copyright 2008, Mark Harrah
*
*	This file is part of Frostbridge.
*
*    Frostbridge is free software: you can redistribute it and/or modify
*    it under the terms of the GNU Lesser General Public License as published by
*    the Free Software Foundation, either version 2.1 of the License, or
*    (at your option) any later version.
*
*    Frostbridge is distributed in the hope that it will be useful,
*    but WITHOUT ANY WARRANTY; without even the implied warranty of
*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*    GNU Lesser General Public License for more details.
*
*    You should have received a copy of the GNU Lesser General Public License
*    along with Frostbridge.  If not, see <http://www.gnu.org/licenses/>.
*/
package net.frostbridge.example

import out._
case class TestExpression(xml: Seq[Node], expectedValue: Option[Double])

case class TreeParameters(avgLength: AverageExpressionLength, var maximumNodes: Int)
{
	def -- =
	{
		maximumNodes -= 1
		this
	}
}

import org.scalacheck.{Arbitrary, Gen, Properties}
object CalculatorSpecification extends Properties("Calculator") with NotNull
{
	val MaximumDepth = 10
	val MaximumNodes = 500
	
	def genExpression(depth: Int, params: => TreeParameters): Gen[TestExpression] =
		if(depth >= MaximumDepth)
			genNumber
		else
			Gen.lzy(Gen.frequency((7, genFunction(depth, params)), (2, genNumber)))
			
	def genExpressions(depth: Int, length: Int, params: TreeParameters): Gen[List[TestExpression]] =
		Gen.containerOfN[List, TestExpression](length, genExpression(depth, params))
	
	implicit val arbExpression = Arbitrary(Gen.frequency(
			(5, genExpression(0, TreeParameters(FixedAverage(1.5), MaximumNodes))),
			(1, genExpression(0, TreeParameters(FixedAverage(10.0), MaximumNodes))),
			(5, genExpression(0, TreeParameters(FunctionMin, MaximumNodes))),
			(5, genExpression(0, TreeParameters(FunctionMax, MaximumNodes)))
		))
	
	private lazy val functions = Calculator.functionTable.values.toList
	def genFunction(depth: Int, params: TreeParameters): Gen[TestExpression] =
	{
		for(f <- Gen.elements(functions: _*); 
			length <- util.Random.poissonGen(params.avgLength(f) );
			arguments <- genExpressions(depth+1, (length min params.maximumNodes) max 0, params--)) yield
		{
			val argumentCount = arguments.length
			val validArgumentCount = f.minArgs <= argumentCount && f.maxArgs >= argumentCount
			val expectedValue =
				if(validArgumentCount && !arguments.exists(_.expectedValue.isEmpty))
				{
					val doubleArguments = arguments.map(_.expectedValue.get)
					try {
						Some(f.eval(doubleArguments))
					}
					catch {
						case e: Exception => None
					}
				}
				else
					None
			val nodes = arguments.flatMap(_.xml)
			TestExpression(Element(f.name, nodes) :: Nil, expectedValue)
		}
	}
	val genNumber: Gen[TestExpression] =
	{
		for(num <- Arbitrary.arbDouble.arbitrary) yield
			TestExpression(Element(Calculator.numberName, Text(num.toString) :: Nil) :: Nil, Some(num))
	}
	
	specify("correctly evaluated", (exp: TestExpression) => testExpression(exp))
	
	def testExpression(exp: TestExpression): Boolean =
	{
		val expected = exp.expectedValue
		TestPattern(Calculator.exp, exp.xml, expected.isEmpty, false,
			(v: Double) => expected.isDefined && sameDouble(expected.get, v),
			(s: String) => true)
	}
	private def sameDouble(a: Double, b: Double) =
		(a.isNaN && b.isNaN) || a == b
	
}

sealed trait AverageExpressionLength extends NotNull
{
	def apply(f: Calculator.F): Double
}
object FunctionMax extends AverageExpressionLength
{
	def apply(f: Calculator.F) =
	{
		f.maxArgs match
		{
			case Infinite => 10.0
			case Finite(max) => max
		}
	}
}
object FunctionMin extends AverageExpressionLength
{
	def apply(f: Calculator.F) = f.minArgs
}
case class FixedAverage(value: Double) extends AverageExpressionLength
{
	def apply(f: Calculator.F) = value
}
