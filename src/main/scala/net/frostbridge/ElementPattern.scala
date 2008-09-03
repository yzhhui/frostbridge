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

trait ElementOp[Generated, ChildGenerated] extends NotNull
{
	/** Given the name and the generated object, generate the element content, or None
	* if the provided object is invalid. */
	def marshalTranslate(generatedName: QName, g: Generated): Option[ChildGenerated]
	
	/** A function that processes the result of the child pattern. */
	def generate(matchedName: QName, childResult: ChildGenerated): Generated
	
	/** A function that generates the name of the marshalled element from the generated object. */
	def generateName(g: Generated): Option[QName]
}
abstract class EmptyElementOp[Generated] extends ElementOp[Generated, Unit]
{
	def generate(matchedName: QName): Generated
	
	final def marshalTranslate(generatedName: QName, g: Generated) = Some(g)
	final def generate(matchedName: QName, u: Unit): Generated = generate(matchedName)
}
abstract class TextElementOp[Generated] extends ElementOp[Generated, Generated]
{
	final def marshalTranslate(generatedName: QName, g: Generated) = Some(g)
	final def generate(matchedName: QName, g: Generated) = g
}
abstract class NamedElementOp[Generated, ChildGenerated] extends ElementOp[Generated, ChildGenerated]
{
	def name: QName
	def generate(g: ChildGenerated): Generated
	def marshalTranslate(g: Generated): Option[ChildGenerated]
	
	def generateName(g: Generated) = Some(name)
	final def generate(matchedName: QName, g: ChildGenerated): Generated = generate(g)
	final def marshalTranslate(generatedName: QName, g: Generated): Option[ChildGenerated] = marshalTranslate(g)
}

/**
* A pattern that matches an element based on its name, attributes, child elements, and text content.
*/
private abstract class ElementPattern[Generated, ChildGenerated] extends UnmatchedPattern[Generated]
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
	
	def op: ElementOp[Generated, ChildGenerated]
	/*
	def marshalTranslate(generatedName: QName, g: Generated): Option[ChildGenerated]
	
	/**
	* A function that processes the result of the child pattern.
	*/
	def generate(matchedName: QName, childResult: ChildGenerated): Generated
	
	def generateName(g: Generated): Option[QName]*/
	
	private[frostbridge] final def deriveImpl(node: in.Node)(implicit o: Optimize): Pattern[Generated] =
	{
		node match
		{
			case open: in.Open =>
			{
				val actualName = open.elementName
				if(nameClass.matches(actualName))
					content(actualName, childrenPattern, op)
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
			for(name <- orError(g, op.generateName(g));
				childValue <- orError(g, op.marshalTranslate(name, g));
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
private abstract class EmptyElementPattern[Generated] extends ElementPattern[Generated, Unit]
{
	def childrenPattern = EmptyElementPattern.empty
	def op: EmptyElementOp[Generated]
}
private final case class SimpleEmptyElementPattern[Generated]
	(nameClass: Name, value: Generated) extends EmptyElementPattern[Generated]
{
	val op =
		new EmptyElementOp[Generated]
		{
			def generate(matchedName: QName) = value
			def generateName(g: Generated) = Some(nameClass.name)
		}
	lazy val hash = List(getClass, nameClass, value).hashCode
}
private final case class AdvancedEmptyElementPattern[Generated]
	(nameClass: NameClass, op: EmptyElementOp[Generated]) extends EmptyElementPattern[Generated]
{
	lazy val hash = List(getClass, nameClass, op).hashCode
}

private final class SimpleElementPattern[Generated, ChildGenerated]
	(val childrenPattern: Pattern[ChildGenerated], val op: NamedElementOp[Generated, ChildGenerated])
	extends ElementPattern[Generated, ChildGenerated]
{
	def nameClass = Name(op.name)
	lazy val hash = List(getClass, childrenPattern, op).hashCode
	override def equals(o: Any) =
	{
		o match
		{
			case e: SimpleElementPattern[_, _] => e.op == op && (childrenPattern eq e.childrenPattern)
			case _ => false
		}
	}
}

private final class AdvancedElementPattern[Generated, ChildGenerated]
	(val nameClass: NameClass, val childrenPattern: Pattern[ChildGenerated], val op: ElementOp[Generated, ChildGenerated])
	extends ElementPattern[Generated, ChildGenerated]
{
	lazy val hash = List(getClass, nameClass, childrenPattern, op).hashCode
	override def equals(o: Any) =
	{
		o match
		{
			case e: AdvancedElementPattern[_, _] =>
				e.nameClass == nameClass && (childrenPattern eq e.childrenPattern) && op == e.op
			case _ => false
		}
	}
}
private final case class SimpleRecursiveElementPattern[Generated, ChildGenerated]
	(childrenPatternA: Unit => Pattern[ChildGenerated], op: NamedElementOp[Generated, ChildGenerated])
	extends ElementPattern[Generated, ChildGenerated]
{
	def nameClass = Name(op.name)
	lazy val childrenPattern = childrenPatternA(())
	lazy val hash = List(getClass, childrenPatternA).hashCode
	
}
private final case class AdvancedRecursiveElementPattern[Generated, ChildGenerated]
	(nameClass: NameClass, childrenPatternA: Unit => Pattern[ChildGenerated], op: ElementOp[Generated, ChildGenerated])
	extends ElementPattern[Generated, ChildGenerated]
{
	lazy val childrenPattern = childrenPatternA(())
	lazy val hash = List(getClass, nameClass, childrenPatternA, op).hashCode
}

private[frostbridge] trait ElementPatternFactory
{
	final def emptyElement[Generated](name: Name, value: Generated)(implicit o: Optimize): Pattern[Generated] =
		o.intern(SimpleEmptyElementPattern(name, value))
	
	final def generalEmptyElement[Generated](nameClass: NameClass, op: EmptyElementOp[Generated])
		(implicit o: Optimize): Pattern[Generated] = o.intern(AdvancedEmptyElementPattern[Generated](nameClass, op))

	final def element[Generated, ChildGenerated] (childrenPattern: Pattern[ChildGenerated],
		op: NamedElementOp[Generated, ChildGenerated])(implicit o: Optimize): Pattern[Generated] =
			o.intern(new SimpleElementPattern[Generated, ChildGenerated](o.reduce(childrenPattern), op))

	final def generalElement[Generated, ChildGenerated](nameClass: NameClass, childrenPattern: Pattern[ChildGenerated],
		op: ElementOp[Generated, ChildGenerated])(implicit o: Optimize): Pattern[Generated] =
			o.intern(new AdvancedElementPattern[Generated, ChildGenerated](nameClass, o.reduce(childrenPattern), op))
		
	final def recursiveElement[Generated, ChildGenerated] (childrenPattern: Unit => Pattern[ChildGenerated],
		op: NamedElementOp[Generated, ChildGenerated])(implicit o: Optimize): Pattern[Generated] =
			o.intern(SimpleRecursiveElementPattern[Generated, ChildGenerated](childrenPattern, op))

	final def recursiveGeneralElement[Generated, ChildGenerated](nameClass: NameClass,
		childrenPattern: Unit => Pattern[ChildGenerated], op: ElementOp[Generated, ChildGenerated])
		(implicit o: Optimize): Pattern[Generated] =
			o.intern(AdvancedRecursiveElementPattern[Generated, ChildGenerated](nameClass, childrenPattern, op))
		
	final def textElement[Generated](name: Name, textContent: ValueParser[Generated])(implicit o: Optimize): Pattern[Generated] =
		o.intern(SimpleTextElementPattern[Generated](name, textContent))
	
	final def generalTextElement[Generated]
		(nameClass: NameClass, textContent: ValueParser[Generated], op: TextElementOp[Generated])
		(implicit o: Optimize): Pattern[Generated] =
			o.intern(AdvancedTextElementPattern[Generated](nameClass, textContent, op))
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
	val op = new TextElementOp[Generated] { def generateName(g: Generated) = Some(nameClass.name) }
	lazy val hash = List(getClass, nameClass, textContent).hashCode
}
private final case class AdvancedTextElementPattern[Generated]
	(nameClass: NameClass, textContent: ValueParser[Generated], op: TextElementOp[Generated])(implicit o: Optimize)
	extends TextElementPattern[Generated]()(o)
{
	lazy val hash = List(getClass, nameClass, textContent, op).hashCode
}

private object ElementContentPattern
{
	def content[Generated, ChildGenerated](matchedName: QName, contentPattern: Pattern[ChildGenerated],
		op: ElementOp[Generated, ChildGenerated])(implicit o: Optimize): Pattern[Generated] =
	{
		val pattern = o.reduce(contentPattern)
		pattern.ifValid { o.intern(new ElementContentPattern[Generated, ChildGenerated](matchedName, pattern, op)) }
	}
}
/**
* A helper pattern that matches the rest of an element after the opening
* angle bracket of a start tag.
*/
private sealed class ElementContentPattern[Generated, Input]
	(val matchedName: QName, val pattern: Pattern[Input], val op: ElementOp[Generated, Input])
	extends UnmatchedPattern[Generated] with MarshalInvalid[Generated]
{
	lazy val hash = List(getClass, matchedName, pattern, op).hashCode
	override def equals(o: Any) =
		o match
		{
			case e: ElementContentPattern[_, _] => (this eq e) || 
				(matchedName == e.matchedName && (pattern eq e.pattern) && (op == e.op))
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
		val derived = content(matchedName, pattern.derive(node), op)
		node match
		{
			case end: in.EndTag =>
				pattern.matchEmpty match
				{
					case Some(value) => derived | emptyPattern(op.generate(matchedName, value))
					case None => derived
				}
			case _ =>
				derived
		}
	}
}