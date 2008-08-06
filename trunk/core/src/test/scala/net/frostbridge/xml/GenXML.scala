package net.frostbridge.xml

import org.scalacheck._
import Prop.forAll
import Gen.{choose, containerOfN, elementsFreq, frequency, Params, value, vectorOf}
import Arbitrary.arbitrary


import javax.xml.namespace.QName
import javax.xml.stream.XMLEventWriter
import javax.xml.stream.events._

import com.ctc.wstx.evt._

import scala.xml.Utility

object ArbitraryXML
{
	import GenXMLEvent._
	def xml(implicit config: XMLConfig): Gen[String] =
	{
		import com.ctc.wstx.stax.WstxOutputFactory
		val factory = new WstxOutputFactory
		factory.configureForXmlConformance
		factory.getConfig.enableAutomaticNamespaces(true)
	
		for(docElement <- element(config.content.rootState)) yield
		{
			import java.io.StringWriter
			val writer = new StringWriter
			val output = factory.createXMLEventWriter(writer)
			
			docElement.foreach(output.add)
			
			output.flush
			writer.toString
		}
	}
	def defaultXML: Gen[String] = xml(defaultXMLConfig)
	
	import GenXMLEvent.{contentConfig, stringConfig}
	val defaultContentConfig: ContentConfig = contentConfig(2.0, 2, 2.0)
	val defaultStringConfig: StringConfig = stringConfig(3.0, 5.0, 3.5, 11.0, 5)
	val defaultNamespaces: Option[Int] = Some(3)
	val defaultXMLConfig: XMLConfig = new XMLConfig(defaultContentConfig, defaultStringConfig, defaultNamespaces)
}

object GenXMLEvent
{
	import GenXMLStrings._

	def qname(localG: Gen[String])(implicit config: XMLConfig): Gen[QName] =
		localG flatMap 
		{
			(local: String) =>
			{
				config.namespace match
				{
					case Some(nsG) => for(ns <- nsG) yield new QName(ns.toString, local)
					case None => Gen { (param: Params) => Some(new QName(local)) }
				}
			}
		}

	def startElement(implicit config: XMLConfig): Gen[StartElement] =
	{
		for(nameString <- qname(name);
			attributeCount <- config.content.attributeCount;
			attributes <- vectorOf(attributeCount, attribute)) yield
			config.factory.createStartElement(nameString, convertIterator(attributes), java.util.Collections.emptyList[Namespace].iterator)
	}
	
	
	def endFromStart(start: StartElement)(implicit config: XMLConfig): EndElement =
		config.factory.createEndElement(start.getName, start.getNamespaces)
	
	def comment(implicit config: XMLConfig): Gen[Comment] =
		for(s <- commonString(config.strings.contentLength)) yield
			config.factory.createComment(s)
	
	def processingInstruction(implicit config: XMLConfig): Gen[ProcessingInstruction] =
		for(target <- name;
			data <- commonString(config.strings.contentLength)) yield
			config.factory.createProcessingInstruction(target, data)
	
	def attribute(implicit config: XMLConfig): Gen[Attribute] =
		for(nameString <- (qname(name) suchThat { "xmlns" != _.getPrefix });
			value <- commonString(config.strings.contentLength)) yield
			config.factory.createAttribute(nameString, value)
			
	def characters(implicit config: XMLConfig): Gen[Characters] =
		for(contentString <- commonString(config.strings.contentLength); cdata <- config.strings.cdata) yield
		{
			if(cdata)
				config.factory.createCData(contentString)
			else
				config.factory.createCharacters(contentString)
		}
		
	def element(state: ContentState)(implicit config: XMLConfig): Gen[List[XMLEvent]] =
		for(start <- startElement; contentList <- content(state)) yield
		{
			val end = endFromStart(start)
			start :: contentList ::: end :: Nil
		}
	
	def node(state: ContentState)(implicit config: XMLConfig): Gen[List[XMLEvent]] =
		frequency( (20, element(state) ), (11, nonElementNode.map(List(_))) )
	
	def nonElementNode(implicit config: XMLConfig): Gen[XMLEvent] =
		frequency(
			(7, characters),
			(1, processingInstruction),
			(2, comment) )
	
	def content(state: ContentState)(implicit config: XMLConfig): Gen[List[XMLEvent]] =
	{
		config.content.children.flatMap(childCount => state.elementsAllowed.flatMap(elementsAllowed =>
			if(elementsAllowed)
			{
				for(nodes <- vectorOf(childCount, node(state.down))) yield
					nodes.flatten[XMLEvent]
			}
			else
				vectorOf(childCount, nonElementNode)
		))
	}
	
	private def convertIterator[T](i: Iterable[T]): java.util.Iterator[T] = new IteratorWrapper(i)
	private class IteratorWrapper[T](val wrapped: Iterable[T]) extends java.util.Iterator[T]
	{
		private val i = wrapped.elements
		def hasNext = i.hasNext
		def next = i.next
		def remove = throw new UnsupportedOperationException
	}
	
	def average(value: Int): Gen[Boolean] =
	{
		assume(value > 0)
		elementsFreq( (value - 1, false), (1, true) )
	}
	def poisson(expected: Double): Gen[Int] =
		Gen[Int]
		{
			(params: Params) =>
			{
				val L = Math.exp(-expected)
				def poissonImpl(k: Int, p: Double): Int =
				{
					if(p < L)
						k
					else
						poissonImpl(k+1, p * StdRand.choose(0.0, 1.0))
				}
				Some(poissonImpl(0, StdRand.choose(0.0, 1.0)))
			}
		}
	def contentConfig(averageChildren: Double, averageDepth: Int, averageAttributes: Double) =
		new ContentConfig(poisson(averageChildren), average(averageDepth), poisson(averageAttributes), 2 * averageDepth)
	def stringConfig(averageContentLength: Double, averageNameLength: Double, averageSchemeLength: Double,
			averageSSPLength: Double, expectedCData: Int) =
		new StringConfig(poisson(averageContentLength), poisson(averageNameLength), poisson(averageSchemeLength),
			poisson(averageSSPLength), average(expectedCData))
}

object GenXMLStrings
{
	def name(implicit config: XMLConfig): Gen[String] =
	{
		val (startG, mainG) =
			if(config.namespaces_?) (ncnameStart, ncnameChar) else (nameStart, nameChar)
		for(length <- config.strings.nameLength; start <- startG; main <- vectorOf(length-1, mainG)) yield
			new String((start :: main).toArray)
	}
		
	def nonEmptyString(implicit lengthG: Gen[Int]): Gen[String] = string(common, 1)
	def alphaLowerString(implicit lengthG: Gen[Int]): Gen[String] = string(lowercaseLetter, 1)
	def commonString(implicit lengthG: Gen[Int]): Gen[String] = string(common, 0)
		
	def string(g: Gen[Char], minimumLength: Int)(implicit lengthG: Gen[Int]): Gen[String] = 
		for(length <- lengthG; array <- containerOfN[Array,Char](length+minimumLength, g)) yield
			new String(array)
		
	
	def nameStart: Gen[Char] = frequency( (50, lowercaseLetter), (10, uppercaseLetter), (1, underscore) )

	def ncnameChar: Gen[Char] =
		frequency( (50, lowercaseLetter), (10, uppercaseLetter), (7, digit),
			(5, underscore), (2, value('.')) )
			
	def common: Gen[Char] = frequency( (2, space), (11, printable) )
	
	def space: Gen[Char] = elementsFreq( (1, '\t'), (4, '\n'), (2, '\r'), (15, ' ') )
	def printable: Gen[Char] =
		frequency((50, lowercaseLetter), (10, uppercaseLetter),
			(1, choose(33,126).map(_.asInstanceOf[Char])), (3, digit))
		
	def digit: Gen[Char] = Gen.choose(0,9).map(java.lang.Character.forDigit(_, 10))
	def letter: Gen[Char] = frequency( (1, lowercaseLetter), (1, uppercaseLetter) )
	def lowercaseLetter: Gen[Char] = letterCase(0x60)
	def uppercaseLetter: Gen[Char] = letterCase(0x40)
	private def letterCase(offset: Int): Gen[Char] =
	{
		for(letter <- Gen.choose(1, 26)) yield
			(letter + offset).asInstanceOf[Char]
	}
			
	def underscore: Gen[Char] = value('_')
	
	def ncnameStart: Gen[Char] = nameStart
	def nameChar: Gen[Char] = frequency( (74, ncnameChar), (1, value(':')) )
	
}