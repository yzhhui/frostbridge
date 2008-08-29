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

object NoOptimizePatternGen extends PatternGen with NoOptimizeGen with DefaultPatternFrequency

object FullOptimizePatternGen extends PatternGen with NoOptimizeGen with DefaultPatternFrequency

trait NoOptimizeGen extends PatternGen
{
	protected val optimization = new NoOptimize
}
trait FullOptimizeGen extends PatternGen
{
	protected val optimization = new InternedDeriveOptimize
}
trait DefaultPatternFrequency extends PatternGen
{
	def attributeFrequency = 5
	def elementFrequency = 1
	def choiceFrequency = 3
	def sequenceFrequency = 3
	def textFrequency = 2
}

abstract class PatternGen extends NotNull
{
	def attributeFrequency: Int
	def elementFrequency: Int
	def choiceFrequency: Int
	def sequenceFrequency: Int
	def textFrequency: Int

	protected val optimization: Optimize
	implicit def optimize: Optimize = optimization

	implicit val namespaceGenerator = xml.ArbitraryXML.defaultNamespaces.namespace

	implicit lazy val arbElement = Arbitrary(anyElement)
	implicit lazy val arbAttribute = Arbitrary(anyAttribute)
	implicit lazy val arbText = Arbitrary(anyText)
	implicit lazy val arbChoice = Arbitrary(anyChoice)
	implicit lazy val arbSequence = Arbitrary(anySequence)
	
	val anySequence = genSequence(genAny, genAny)
	val anyChoice = genChoice(genAny)
	val anyElement = genElement(anyNameClass, genAny)
	val anyAttribute = genAttribute(anyNameClass, anyValue)
	val anyText = genText(anyValue)
	
	def genAny: Gen[FTestPattern] = genAnyImpl
	private lazy val genAnyImpl: Gen[FTestPattern] =
	{
		Gen.lzy(Gen.frequency(
			(choiceFrequency, anyChoice),
			(sequenceFrequency, anySequence),
			(elementFrequency, anyElement),
			(attributeFrequency, anyAttribute),
			(textFrequency, anyText)
		))
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
	
	def anyValue: Gen[FTestValueParser] = Gen.frequency( (1, booleanValue), (1,  doubleValue), (1, integerValue))
	def genValue[G](arbitrary: Arbitrary[G], valueParser: ValueParser[G]): Gen[FTestValueParser] =
	{
		val gen = for(value <- arbitrary.arbitrary) yield FTestString(value.toString, true)
		Gen.value(FTestValueParser(valueParser, gen))
	}
	def booleanValue = genValue(Arbitrary.arbBool, BooleanValue)
	def doubleValue = genValue(Arbitrary.arbDouble, AnyDouble)
	def integerValue = genValue(Arbitrary.arbInt, AnyInteger)

	def genChoice(implicit contentGen: Gen[FTestPattern]): Gen[FTestPattern] =
	{
		for(a <- contentGen; b <- contentGen; selectFragment <- Arbitrary.arbitrary[Boolean]) yield
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
	def genSequence(implicit contentGen1: Gen[FTestPattern], contentGen2: Gen[FTestPattern]): Gen[FTestPattern] =
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
				val pattern = PatternFactory.attribute(name.name, valueParser, (g: G) => None)
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
				val pattern = PatternFactory.generalElement(name.name, contentPattern, (q: QName, g: G) => g, (q: QName, g: G) => None, (g: G) => None)
				val xml =
					for(testName <- name.testName; testContent <- content.testXML) yield
						FTestXML(Element(testName.name, testContent.fragment) :: Nil, testName.valid && testContent.valid)
				FTestPattern(pattern, xml)
			}
			genImpl(content.pattern)
		}
	}
	
	def genText(implicit valueGen: Gen[FTestValueParser]): Gen[FTestPattern] =
	{
		for(value <- valueGen) yield
		{
			val pattern = PatternFactory.textPattern(value.value)
			val xml =
				for(testValue <- value.testContent) yield
					FTestXML(Text(testValue.content) :: Nil, testValue.valid)
			FTestPattern(pattern, xml)
		}
	}
}