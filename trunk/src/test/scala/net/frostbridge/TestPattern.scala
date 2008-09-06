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
package net.frostbridge

import out._
import data._
import org.scalacheck.{Arbitrary, Gen}

case class FTestPattern(pattern: Pattern[_], testXML: Gen[FTestXML]) extends NotNull
{
	override def toString = "Test Pattern:\n" + Traceable.traceToString(pattern)
}
case class FTestValueParser(value: ValueParser[_], testContent: Gen[FTestString]) extends NotNull
case class FTestNameClass(name: NameClass, testName: Gen[FTestQName]) extends NotNull

case class FTestXML(fragment: Seq[Node], valid: Boolean) extends NotNull
case class FTestString(content: String, valid: Boolean) extends NotNull
case class FTestQName(name: QName, valid: Boolean) extends NotNull

abstract class DefaultPatternFrequency extends NotNull
{
	def attributeDataFrequency = 5
	def elementDataFrequency = 1
	def textDataFrequency = 2
	
	def booleanFrequency = 1
	def integerFrequency = 5
	def doubleFrequency = 4
	def stringFrequency = 6
	
	def startingFrequency = 6
	def newFrequency(lastFrequency: Int) = (lastFrequency / 2) + 1
	
	def choiceFrequency = 3
	def unorderedFrequency = 5
	def orderedFrequency = 7
	def elementFrequency = 2
	def repeatFrequency = 1
	
	def infiniteFrequency = 7
	def finiteFrequency = 4
	
	def averageFiniteUpperBound = 50.0
	
	def averageStringLength = 7.0
	def averageChoiceLength = 5.0
	def averageUnorderedLength = 4.0
	def averageOrderedLength = 6.0
	def averageRepeatStart = 1.5
	def averageIterations = 4.0
}

class PatternGen extends DefaultPatternFrequency
{
	import Random._

	implicit val namespaceGenerator = xml.ArbitraryXML.defaultNamespaces.namespace
	
	implicit def arbElement = Arbitrary(Gen.lzy(genAnyElement))
	
	def genAnyElement: Gen[FTestPattern] =
	{
		val initialPatterns = List(
			(booleanFrequency, booleanPattern),
			(integerFrequency, integerPattern),
			(doubleFrequency, doublePattern)
			/*(stringFrequency, stringPattern)*/)
		genElement(anyNameClass, build(initialPatterns, poisson(averageIterations), startingFrequency))
	}
	private def build(patterns: List[(Int, Gen[FTestPattern])], iterations: Int, nextFrequency: Int): Gen[FTestPattern] =
	{
		require(patterns.length >= 2)
		if(iterations == 0)
			patterns.head._2
		else
		{
			val newGen: Gen[FTestPattern] =
				pick1(List(
					(choiceFrequency, choicesList(pick1(patterns), poisson(averageChoiceLength))),
					(unorderedFrequency, unorderedList(pickN(poisson(averageUnorderedLength), patterns))),
					(orderedFrequency, orderedList(pickN(poisson(averageOrderedLength), patterns))),
					//(repeatFrequency, genRepeat(pick1(patterns), poissonGen(averageRepeatStart), genUpperBound)),
					(elementFrequency, genElement(anyNameClass, pick1(patterns)))
				))
			build((nextFrequency, newGen) :: patterns, iterations - 1, newFrequency(nextFrequency))
		}
	}
	def unorderedList(patterns: List[Gen[FTestPattern]]): Gen[FTestPattern] = sequence(patterns, genUnordered(_,_))
	def orderedList(patterns: List[Gen[FTestPattern]]): Gen[FTestPattern] = sequence(patterns, genOrdered(_,_))
	
	private def sequence(patterns: List[Gen[FTestPattern]],
		combine: (Gen[FTestPattern], Gen[FTestPattern]) => Gen[FTestPattern]): Gen[FTestPattern] =
	{
		patterns match
		{
			case Nil => emptyGen
			case head :: Nil => head
			case _ => 
				patterns.reduceLeft( (p1: Gen[FTestPattern], p2: Gen[FTestPattern]) => combine(p1, p2))
		}
	}
	def emptyGen: Gen[FTestPattern] = Gen.value(FTestPattern(EmptyPattern(()), Gen.value(FTestXML(Nil,true)) ) )
			
	def choicesTree(pattern: Gen[FTestPattern], depth: Int): Gen[FTestPattern] =
	{
		if(depth <= 0)
			pattern
		else
			genChoice(choicesTree(pattern, depth-1), choicesTree(pattern, depth-1))
	}
	def choicesList(pattern: Gen[FTestPattern], num: Int): Gen[FTestPattern] =
	{
		if(num <= 0)
			pattern
		else
			genChoice(pattern, choicesList(pattern, num-1))
	}
	
	def anyNameClass: Gen[FTestNameClass] = Gen.frequency( (1, anyName) )
	def anyName: Gen[FTestNameClass] = genName(anyQName(namespaceGenerator, xml.ArbitraryXML.defaultStringConfig.nameLength))
	
	def anyQName(implicit namespaceGen: Option[Gen[String]], nameLength: Gen[Int]): Gen[FTestQName] =
	{
		xml.GenXMLStrings.name(true, nameLength) flatMap
		{
			(local: String) =>
			{
				namespaceGen match
				{
					case Some(nsG) => for(ns <- nsG) yield FTestQName(QName(ns, local), true)
					case None => Gen.value(FTestQName(QName("", local), true))
				}
			}
		}
	}
	def genQName(implicit uriGen: Gen[String], localGen: Gen[String]): Gen[FTestQName] =
	{
		for(uri <- uriGen; local <- localGen) yield
			FTestQName(QName(uri, local), true)
	}
	def genName(implicit genQName: Gen[FTestQName]): Gen[FTestNameClass] =
	{
		for(name <- genQName) yield
			FTestNameClass(Name(name.name), Gen.value(FTestQName(name.name, true)))
	}
	
	def genValue[G](arbitrary: Arbitrary[G], valueParser: ValueParser[G]): Gen[FTestValueParser] =
	{
		val gen = for(value <- arbitrary.arbitrary) yield FTestString(value.toString, true)
		Gen.value(FTestValueParser(valueParser, gen))
	}
	def booleanValue = genValue(Arbitrary.arbBool, BooleanValue)
	def doubleValue = genValue(Arbitrary.arbDouble, AnyDouble)
	def integerValue = genValue(Arbitrary.arbInt, AnyInteger)
	def stringValue = genValue(Arbitrary(xml.GenXMLStrings.nonEmptyString(poissonGen(averageStringLength))), NonEmptyString)
	
	def genDataPattern[G](valueParser: Gen[FTestValueParser]) =
		Gen.frequency((textDataFrequency, genText(valueParser)),
			(attributeDataFrequency, genAttribute(anyNameClass, valueParser)),
			(elementDataFrequency, genTextElement(anyNameClass, valueParser)) )
			
	val booleanPattern: Gen[FTestPattern] = genDataPattern(booleanValue)
	val integerPattern: Gen[FTestPattern] = genDataPattern(integerValue)
	val doublePattern: Gen[FTestPattern] = genDataPattern(doubleValue)
	val stringPattern: Gen[FTestPattern] = genDataPattern(stringValue)

	def genRepeat(implicit contentGen: Gen[FTestPattern], startGen: Gen[Int], endGen: Gen[UpperBound]): Gen[FTestPattern] =
	{
		contentGen.flatMap {
			(a: FTestPattern) =>
			{
				def fold(t1: FTestXML, t2: FTestXML): FTestXML =
					FTestXML(t1.fragment ++ t2.fragment, t1.valid && t2.valid && validSequence(t1.fragment, t2.fragment))
				val pattern = a.pattern
				for(start <- startGen; end <- endGen; length <- genInBounds(start, end)) yield
				{
					val testXML =
						for(seq <- Gen.containerOfN[List,FTestXML](length, a.testXML)) yield
							seq.foldLeft(FTestXML(Nil, true))(fold)
					FTestPattern(pattern(start, end), testXML)
				}
			}
		}
	}
	def genInBounds(lower: Int, upper: UpperBound): Gen[Int] =
	{
		require(lower >= 0)
		val u =
			upper match
			{
				case Infinite => Math.MAX_INT
				case Finite(f) => f
			}
		Gen.choose(lower, u)
	}
	def genUpperBound: Gen[UpperBound] =
		Gen.frequency(
			(finiteFrequency, for(f <- poissonGen(averageFiniteUpperBound)) yield Finite(f)),
			(infiniteFrequency, Gen.value(Infinite))
		)
		
	def genChoice(implicit contentGen1: Gen[FTestPattern], contentGen2: Gen[FTestPattern]): Gen[FTestPattern] =
	{
		for(a <- contentGen1; b <- contentGen2; selectFragment <- Arbitrary.arbitrary[Boolean]) yield
		{
			val pattern = a.pattern.asInstanceOf[Pattern[Any]] | b.pattern.asInstanceOf[Pattern[Any]]
			val xml =
				for(xmlA <- a.testXML; xmlB <- b.testXML) yield
				{
					val (fragment, valid) = 
						if(xmlA.valid)
						{
							if(xmlB.valid)
								(if(selectFragment) xmlA.fragment else xmlB.fragment, true)
							else
								(xmlA.fragment, true)
						}
						else if(xmlB.valid)
							(xmlB.fragment, true)
						else
							(xmlA.fragment, false)
					FTestXML(fragment, valid)
				}
			FTestPattern(pattern, xml)
		}
	}
	
	def genUnordered(implicit contentGen1: Gen[FTestPattern], contentGen2: Gen[FTestPattern]): Gen[FTestPattern] =
	{
		for(a <- contentGen1; b <- contentGen2; order <- Arbitrary.arbitrary[Boolean]) yield
		{
			val pattern = a.pattern +++ b.pattern
			val xml =
				for(xmlA <- a.testXML; xmlB <- b.testXML) yield
				{
					val aFragment = xmlA.fragment
					val bFragment = xmlB.fragment
					val bothValid = xmlA.valid && xmlB.valid
					if(order)
						FTestXML(aFragment ++ bFragment, bothValid && validSequence(aFragment, bFragment))
					else
						FTestXML(bFragment ++ aFragment, bothValid && validSequence(bFragment, aFragment))
				}
			FTestPattern(pattern, xml)
		}
	}
	def genOrdered(implicit contentGen1: Gen[FTestPattern], contentGen2: Gen[FTestPattern]): Gen[FTestPattern] =
	{
		for(a <- contentGen1; b <- contentGen2) yield
		{
			val pattern = a.pattern :+: b.pattern
			val xml =
				for(xmlA <- a.testXML; xmlB <- b.testXML) yield
				{
					val aFragment = xmlA.fragment
					val bFragment = xmlB.fragment
					FTestXML(aFragment ++ bFragment, xmlA.valid && xmlB.valid && validSequence(aFragment, bFragment))
				}
			FTestPattern(pattern, xml)
		}
	}
	private def validSequence(a: Seq[Node], b: Seq[Node]) =
	{
		a.isEmpty || b.isEmpty ||
		{
			{
				def removeLeadingAttributes(s: Seq[Node]): Seq[Node] = s.dropWhile(n => n.isInstanceOf[Attribute])
				val tailAttributesRemoved = removeLeadingAttributes(a.reverse)
				val headAttributesRemoved = removeLeadingAttributes(b)
				
				tailAttributesRemoved.isEmpty || headAttributesRemoved.isEmpty ||
					!(tailAttributesRemoved.first.isInstanceOf[Text] && headAttributesRemoved.first.isInstanceOf[Text])
			} &&
			a.forall(an => b.forall(bn => differentAttributeNames(an, bn)))
		}
	}
	private def differentAttributeNames(a: Node, b: Node) =
	{
		a match
		{
			case Attribute(aName, _) =>
			{
				b match
				{
					case Attribute(bName, _) => aName != bName
					case _ => true
				}
			}
			case _ => true
		}
	}
	
	def genAttribute(implicit nameGen: Gen[FTestNameClass], valueGen: Gen[FTestValueParser]): Gen[FTestPattern] =
	{
		for(name <- nameGen; value <- valueGen) yield
		{
			// unpack existential
			def genImpl[G](valueParser: ValueParser[G]): FTestPattern =
			{
				import PatternFactory.attribute
				val pattern =
					name.name match
					{
						case Name(q) => attribute(q, valueParser)
						case other => 
							new GeneralAttributePattern(other, valueParser)
							{
								def generateName(g: G) = None
							}
					}
				val xml =
					for(testName <- name.testName; testValue <- value.testContent) yield
						FTestXML(Attribute(testName.name, testValue.content) :: Nil, testName.valid && testValue.valid)
				FTestPattern(pattern, xml)
			}
			genImpl(value.value)
		}
	}
	
	def genElement(implicit nameGen: Gen[FTestNameClass], contentGen: Gen[FTestPattern]): Gen[FTestPattern] =
	{
		for(name <- nameGen; content <- contentGen) yield
		{
			// unpacks the existential
			def genImpl[G](contentPattern: Pattern[G]): FTestPattern =
			{
				val pattern = 
				{
					import PatternFactory._
					name.name match
					{
						case Name(q) => 
							new NamedElementPattern[G,G](q, contentPattern)
							{
								def generate(g: G) = g
								def marshalTranslate(g: G) = Some(g)
							}
						case other =>
							new GeneralElementPattern[G,G](other, contentPattern)
							{
								def generate(q: QName, g: G) = g
								def marshalTranslate(q: QName, g: G) = None
								def generateName(g: G) = None
							}
					}
				}
				val xml =
					for(testName <- name.testName; testContent <- content.testXML) yield
						FTestXML(Element(testName.name, testContent.fragment) :: Nil, testName.valid && testContent.valid)
				FTestPattern(pattern, xml)
			}
			genImpl(content.pattern)
		}
	}
	def genTextElement(implicit nameGen: Gen[FTestNameClass], valueGen: Gen[FTestValueParser]): Gen[FTestPattern] =
	{
		for(name <- nameGen; value <- valueGen) yield
		{
			// unpacks the existential
			def genImpl[G](valueParser: ValueParser[G]): FTestPattern =
			{
				val pattern = 
				{
					import PatternFactory._
					name.name match
					{
						case Name(q) => TextElement(q, valueParser)
						case other =>
							new GeneralTextElementPattern(other, valueParser)
							{
								def generateName(g: G) = None
							}
					}
				}
				val xml =
					for(testName <- name.testName; testContent <- value.testContent) yield
						FTestXML(Element(testName.name, List(Text(testContent.content))) :: Nil, testName.valid && testContent.valid)
				FTestPattern(pattern, xml)
			}
			genImpl(value.value)
		}
	}
	
	def genText(implicit valueGen: Gen[FTestValueParser]): Gen[FTestPattern] =
	{
		for(value <- valueGen) yield
		{
			val pattern = TextPattern(value.value)
			val xml =
				for(testValue <- value.testContent) yield
					FTestXML(Text(testValue.content) :: Nil, testValue.valid)
			FTestPattern(pattern, xml)
		}
	}
	
}

object Random
{
	import org.scalacheck._
	def poisson(expected: Double): Int =
	{
		val L = Math.exp(-expected)
		def poissonImpl(k: Int, p: Double): Int =
		{
			if(p < L)
				k
			else
				poissonImpl(k+1, p * StdRand.choose(0.0, 1.0))
		}
		poissonImpl(0, StdRand.choose(0.0, 1.0))
	}
	def poissonGen(expected: Double): Gen[Int] = Gen[Int] { (params: Gen.Params) => Some(poisson(expected)) }
	def pickN[T](n: Int, frequencyValues: Seq[(Int, T)]): List[T] =
	{
		require(n >= 0)
		val cumulative = accumulate(frequencyValues)
		def doPick(remaining: Int, workingList: List[T]): List[T] =
			if(remaining <= 0)
				workingList
			else
				doPick(remaining -1, pick(cumulative) :: workingList)
			
		doPick(n, Nil)
	}
	def pick1[T](frequencyValues: Seq[(Int, T)]): T = pick(accumulate(frequencyValues))
	private def pick[T](cumulative: (Int, List[(Int, T)])): T =
	{
		val (total, list) = cumulative
		require(total >= 1)
		val i = StdRand.choose(1, total)
		list.find(i <= _._1).get._2
	}
	private def accumulate[T](frequencyValues: Seq[(Int, T)]): (Int, List[(Int,T)]) =
	{
		val size = frequencyValues.size
		require(size != 0)
		def fold(result: (Int, List[(Int, T)]), current: (Int, T)): (Int, List[(Int, T)]) =
		{
			val newSum = result._1 + current._1
			(newSum, (newSum, current._2) :: result._2)
		}
		val reverse = ( (0, Nil: List[(Int, T)]) /: frequencyValues )(fold)
		(reverse._1, reverse._2.reverse)
	}
}