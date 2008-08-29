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

import util.TList
import ElementContentPattern._
import PatternImpl._
import PatternFactory._
import Traceable._
import data.ValueParser
import java.io.Writer

/**
* A pattern that matches an element based on its name, attributes, child elements, and text content.
*/
private trait ElementPattern[Generated, ChildGenerated] extends UnmatchedPattern[Generated]
	with ReferencedTraceable with MarshalErrorTranslator[Generated]
{
	/**
	* Describes the set of allowed names of an element matched by this pattern.
	*/
	def nameClass: NameClass
	/**
	* The child pattern of this element.  This pattern will match attributes, child elements, and/or
	* text content.  Use a compound pattern for multiple children and EmptyPattern for no content.
	*/
	def childrenPattern: Pattern[ChildGenerated]
	def marshalTranslate(generatedName: QName, g: Generated): Option[ChildGenerated]
	
	/**
	* A function that processes the result of the child pattern.
	*/
	def generate(matchedName: QName, childResult: ChildGenerated): Generated
	
	def generateName(g: Generated): Option[QName]
	
	private[frostbridge] final def deriveImpl(node: in.Node)(implicit o: Optimize): Pattern[Generated] =
	{
		node match
		{
			case open: in.Open =>
			{
				val actualName = open.elementName
				if(nameClass.matches(actualName))
					content(actualName, childrenPattern, this)
				else
					NotAllowedPattern
			}
			case close: in.Close => this
			case _ => NotAllowedPattern
		}
	}
	
	protected[this] def orError[B](g: Generated, value: Option[B]): Either.RightProjection[RootMarshalException[Generated],B] =
		value.toRight(RootMarshalException(g, this)).right
	
	def marshal(g: Generated, reverseXML: TList[out.Node]) =
	{
		translateMarshalError(g)
		{
			for(name <- orError(g, generateName(g));
				childValue <- orError(g, marshalTranslate(name, g));
				content <- childrenPattern.marshal(childValue, TList.empty).right)
			yield
				out.Element(name, content.reverse) :: reverseXML
		}
	}
	
	def matchEmpty = None
	
	def nextPossiblePatterns = List(this)
	def description = "element '" + nameClass.description + "'"
	// for tracing
	def name = "Pattern " + nameClass.description
	def trace(writer: Writer, level: Int, reference: ReferenceFunction) =
	{
		basicTrace(writer, level, "element " + nameClass.description)
		basicTrace(writer, level, "{")
		childrenPattern.embeddedTrace(writer, level+1, reference)
		basicTrace(writer, level, "}")
	}
}

private object EmptyElementPattern
{
	val empty = emptyPattern(())
}
private trait EmptyElementPattern[Generated] extends ElementPattern[Generated, Unit]
{
	def childrenPattern = EmptyElementPattern.empty
	def marshalTranslate(generatedName: QName, g: Generated) = Some(())
}
private final case class SimpleEmptyElementPattern[Generated]
	(nameClass: Name, value: Generated) extends EmptyElementPattern[Generated]
{
	def generate(matchedName: QName, u: Unit) = value
	def generateName(g: Generated) = Some(nameClass.name)
	lazy val hash = List(getClass, nameClass, value).hashCode
}
private final case class AdvancedEmptyElementPattern[Generated]
	(nameClass: NameClass, value: QName => Generated, generateNameA: Generated => Option[QName]) extends EmptyElementPattern[Generated]
{
	def generate(matchedName: QName, u: Unit) = value(matchedName)
	def generateName(g: Generated) = generateNameA(g)
	lazy val hash = List(getClass, nameClass, value, generateNameA).hashCode
}

private final class SimpleElementPattern[Generated, ChildGenerated]
	(val nameClass: Name, val childrenPattern: Pattern[ChildGenerated],
	val generateA: ChildGenerated => Generated,
	val marshalTranslateA: Generated => Option[ChildGenerated])
	extends ElementPattern[Generated, ChildGenerated]
{
	def generateName(g: Generated) = Some(nameClass.name)
	def generate(generatedName: QName, g: ChildGenerated) = generateA(g)
	def marshalTranslate(generatedName: QName, g: Generated) = marshalTranslateA(g)
	lazy val hash = List(getClass, nameClass, childrenPattern, generateA, marshalTranslateA).hashCode
	override def equals(o: Any) =
	{
		o match
		{
			case e: SimpleElementPattern[_, _] =>
				e.nameClass == nameClass && (childrenPattern eq e.childrenPattern) &&
				generateA == e.generateA && marshalTranslateA == e.marshalTranslateA
			case _ => false
		}
	}
}

private final class AdvancedElementPattern[Generated, ChildGenerated]
	(val nameClass: NameClass, val childrenPattern: Pattern[ChildGenerated],
	val generateA: (QName, ChildGenerated) => Generated,
	val marshalTranslateA: (QName, Generated) => Option[ChildGenerated],
	val generateNameA: Generated => Option[QName])
	extends ElementPattern[Generated, ChildGenerated]
{
	def generateName(g: Generated) = generateNameA(g)
	def generate(generatedName: QName, g: ChildGenerated) = generateA(generatedName, g)
	def marshalTranslate(generatedName: QName, g: Generated) = marshalTranslateA(generatedName, g)
	lazy val hash = List(getClass, nameClass, childrenPattern, generateA, marshalTranslateA, generateNameA).hashCode
	override def equals(o: Any) =
	{
		o match
		{
			case e: AdvancedElementPattern[_, _] =>
				e.nameClass == nameClass && (childrenPattern eq e.childrenPattern) &&
				generateA == e.generateA && marshalTranslateA == e.marshalTranslateA && e.generateNameA == generateNameA
			case _ => false
		}
	}
}
private final case class SimpleRecursiveElementPattern[Generated, ChildGenerated]
	(nameClass: Name, childrenPatternA: Unit => Pattern[ChildGenerated],
	generateA: ChildGenerated => Generated,
	marshalTranslateA: Generated => Option[ChildGenerated])
	extends ElementPattern[Generated, ChildGenerated]
{
	lazy val childrenPattern = childrenPatternA(())
	def generateName(g: Generated) = Some(nameClass.name)
	def generate(generatedName: QName, g: ChildGenerated) = generateA(g)
	def marshalTranslate(generatedName: QName, g: Generated) = marshalTranslateA(g)
	lazy val hash = List(getClass, nameClass, childrenPatternA, generateA, marshalTranslateA).hashCode
	
}
private final case class AdvancedRecursiveElementPattern[Generated, ChildGenerated]
	(nameClass: NameClass, childrenPatternA: Unit => Pattern[ChildGenerated],
	generateA: (QName, ChildGenerated) => Generated,
	marshalTranslateA: (QName, Generated) => Option[ChildGenerated],
	generateNameA: Generated => Option[QName])
	extends ElementPattern[Generated, ChildGenerated]
{
	lazy val childrenPattern = childrenPatternA(())
	def generateName(g: Generated) = generateNameA(g)
	def generate(generatedName: QName, g: ChildGenerated) = generateA(generatedName, g)
	def marshalTranslate(generatedName: QName, g: Generated) = marshalTranslateA(generatedName, g)
	lazy val hash = List(getClass, nameClass, childrenPatternA, generateA, marshalTranslateA, generateNameA).hashCode
}

private[frostbridge] trait ElementPatternFactory
{
	final def emptyElement[Generated](name: Name, value: Generated)(implicit o: Optimize): Pattern[Generated] =
		o.intern(SimpleEmptyElementPattern(name, value))
	
	final def generalEmptyElement[Generated](nameClass: NameClass, value: QName => Generated,
		generateName: Generated => Option[QName])(implicit o: Optimize): Pattern[Generated] =
			o.intern(AdvancedEmptyElementPattern[Generated](nameClass, value, generateName))

	final def element[Generated, ChildGenerated] (name: Name, childrenPattern: Pattern[ChildGenerated],
		generate: (ChildGenerated => Generated),
		marshalTranslate: Generated => Option[ChildGenerated])(implicit o: Optimize): Pattern[Generated] =
			o.intern(new SimpleElementPattern[Generated, ChildGenerated](name, o.reduce(childrenPattern), generate, marshalTranslate))

	final def generalElement[Generated, ChildGenerated](nameClass: NameClass, childrenPattern: Pattern[ChildGenerated],
		generate: (QName, ChildGenerated) => Generated, marshalTranslate: (QName, Generated) => Option[ChildGenerated],
		generateName: Generated => Option[QName])(implicit o: Optimize): Pattern[Generated] =
			o.intern(new AdvancedElementPattern[Generated, ChildGenerated](nameClass, o.reduce(childrenPattern), generate, marshalTranslate, generateName))
		
	final def recursiveElement[Generated, ChildGenerated] (name: Name, childrenPattern: Unit => Pattern[ChildGenerated],
		generate: (ChildGenerated => Generated),
		marshalTranslate: Generated => Option[ChildGenerated])(implicit o: Optimize): Pattern[Generated] =
			o.intern(SimpleRecursiveElementPattern[Generated, ChildGenerated](name, childrenPattern, generate, marshalTranslate))

	final def recursiveGeneralElement[Generated, ChildGenerated](nameClass: NameClass,
		childrenPattern: Unit => Pattern[ChildGenerated],
		generate: (QName, ChildGenerated) => Generated, marshalTranslate: (QName, Generated) => Option[ChildGenerated],
		generateName: Generated => Option[QName])(implicit o: Optimize): Pattern[Generated] =
			o.intern(AdvancedRecursiveElementPattern[Generated, ChildGenerated](nameClass, childrenPattern, generate, marshalTranslate, generateName))
		
	final def textElement[Generated](name: Name, textContent: ValueParser[Generated])(implicit o: Optimize): Pattern[Generated] =
		o.intern(SimpleTextElementPattern[Generated](name, textContent))
	
	final def generalTextElement[Generated]
		(nameClass: NameClass, textContent: ValueParser[Generated],
		generateName: Generated => Option[QName])(implicit o: Optimize): Pattern[Generated] =
			o.intern(AdvancedTextElementPattern[Generated](nameClass, textContent, generateName))
}

private sealed abstract class TextElementPattern[Generated](implicit o: Optimize) extends ElementPattern[Generated, Generated]
{
	def textContent: ValueParser[Generated]
	val childrenPattern = textPattern(textContent)
	def generate(matchedName: QName, textValue: Generated) = textValue
	def marshalTranslate(generatedName: QName, g: Generated) = Some(g)
}
private final case class SimpleTextElementPattern[Generated](nameClass: Name, textContent: ValueParser[Generated])(implicit o: Optimize)
	extends TextElementPattern[Generated]()(o)
{
	def generateName(g: Generated) = Some(nameClass.name)
	lazy val hash = List(getClass, nameClass, textContent).hashCode
}
private final case class AdvancedTextElementPattern[Generated]
	(nameClass: NameClass, textContent: ValueParser[Generated], generateNameA: Generated => Option[QName])(implicit o: Optimize)
	extends TextElementPattern[Generated]()(o)
{
	def generateName(g: Generated) = generateNameA(g)
	lazy val hash = List(getClass, nameClass, textContent, generateNameA).hashCode
}

private object ElementContentPattern
{
	def content[Generated, ChildGenerated](matchedName: QName, contentPattern: Pattern[ChildGenerated],
		elementPattern: ElementPattern[Generated, ChildGenerated])
		(implicit o: Optimize): Pattern[Generated] =
	{
		val pattern = o.reduce(contentPattern)
		pattern.ifValid { o.intern(new ElementContentPattern[Generated, ChildGenerated](matchedName, pattern, elementPattern)) }
	}
}
/**
* A helper pattern that matches the rest of an element after the opening
* angle bracket of a start tag.
*/
private sealed class ElementContentPattern[Generated, Input]
	(val matchedName: QName, val pattern: Pattern[Input], val elementPattern: ElementPattern[Generated, Input])
	extends UnmatchedPattern[Generated] with MarshalInvalid[Generated]
{
	lazy val hash = List(getClass, matchedName, pattern, elementPattern).hashCode
	override def equals(o: Any) =
		o match
		{
			case e: ElementContentPattern[_, _] => (this eq e) || 
				(matchedName == e.matchedName && (pattern eq e.pattern) && (elementPattern eq e.pattern))
			case _ => false
		}
	def matchEmpty = None
	
	def nextPossiblePatterns =
	{
		val p1Possible = pattern.nextPossiblePatterns
		if(pattern.matchEmpty.isEmpty)
			p1Possible
		else
			p1Possible :::
			List(
				new Traceable
				{
					def nextPossiblePatterns = List(this)
					def trace(writer: Writer, level: Int, reference: ReferenceFunction) = basicTrace(writer, level, description)
					val description = "element '" + matchedName.description + "' end tag"
				}
			)
	}
	def description = pattern.description + ", element '" + matchedName.description + "' end tag"
	
	def trace(writer: Writer, level: Int, reference: ReferenceFunction) =
	{
		pattern.trace(writer, level, reference)
		basicTrace(writer, level-1, "element '" + matchedName.description + "' end tag")
	}
	
	private[frostbridge] final def deriveImpl(node: in.Node)(implicit o: Optimize) =
	{
		val derived = content(matchedName, pattern.derive(node), elementPattern)
		node match
		{
			case end: in.EndTag =>
				pattern.matchEmpty match
				{
					case Some(value) => derived | emptyPattern(elementPattern.generate(matchedName, value))
					case None => derived
				}
			case _ =>
				derived
		}
	}
}