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

import ElementContentPattern._
import PatternImpl._
import PatternFactory._
import Traceable._
import data.ValueParser
import java.io.Writer

/**
* A pattern that matches an element based on its name, attributes, child elements, and text content.
*/
sealed abstract class ElementPattern[Generated, ChildGenerated]
	extends UnmatchedPattern[Generated] with ReferencedTraceable with MarshalErrorTranslator[Generated]
{
	/**
	* Describes the set of allowed names of an element matched by this pattern.
	*/
	def nameClass: NameClass
	/**
	* The child pattern of this element.  This pattern will match attributes, child elements, and/or
	* text content.  Use a compound pattern for multiple children and EmptyPattern for no content.
	*/
	def childrenPattern(actualName: QName): Pattern[ChildGenerated]
	
	/** Given the name and the generated object, generate the element content, or None
	* if the provided object is invalid. */
	def marshalTranslate(generatedName: QName, g: Generated): Option[ChildGenerated]
	
	/** A function that processes the result of the child pattern. */
	def generate(matchedName: QName, childResult: ChildGenerated): Generated
	
	/** A function that generates the name of the marshalled element from the generated object. */
	def generateName(g: Generated): Option[QName]
	
	final def derive(node: in.Node): Pattern[Generated] =
	{
		node match
		{
			case open: in.Open =>
			{
				val actualName = open.elementName
				if(nameClass.matches(actualName))
					content(actualName, childrenPattern(actualName), this)
				else
					NotAllowedPattern
			}
			case close: in.Close => this
			case _ => NotAllowedPattern
		}
	}
	
	protected[this] def orError[B](g: Generated, value: Option[B]): Either.RightProjection[RootMarshalException[Generated],B] =
		value.toRight(RootMarshalException(g, this)).right
	
	def marshal(g: Generated, reverseXML: List[out.Node]) =
	{
		translateMarshalError(g)
		{
			for(name <- orError(g, generateName(g));
				childValue <- orError(g, marshalTranslate(name, g));
				content <- childrenPattern(name).marshal(childValue, Nil).right)
			yield
				out.Element(name, content.reverse) :: reverseXML
		}
	}
	
	def matchEmpty = None
	
	def nextPossiblePatterns = List(this)
	def description = "element '" + nameClass.description + "'"
	// for tracing
	def label = "Pattern " + nameClass.description
	def trace(writer: Writer, level: Int, reference: ReferenceFunction)
	{
		basicTrace(writer, level, "element " + nameClass.description)
		basicTrace(writer, level, "{")
		traceChildren(writer, level+1, reference)
		basicTrace(writer, level, "}")
	}
	/** This traces the child pattern.  This method is required because childrenPattern
	* can be a function of the matched element name. */
	def traceChildren(writer: Writer, level: Int, reference: ReferenceFunction): Unit
}


abstract class DynamicElementPattern[Generated, ChildGenerated] extends ElementPattern[Generated, ChildGenerated]
{
	def childrenDescription: String
	def traceChildren(writer: Writer, level: Int, reference: ReferenceFunction)
	{
		basicTrace(writer, level, childrenDescription)
	}
}

/** A helper trait for element patterns that match elements with a single, specific name. */
trait NamedElement[Generated, ChildGenerated] extends ElementPattern[Generated, ChildGenerated]
{
	/** The name of elements that this pattern matches. */
	def name: QName
	/** Produces an object given the object produced by this element pattern's children pattern. */
	def generate(g: ChildGenerated): Generated
	/** Translates the given object to the generated type of the children pattern. */
	def marshalTranslate(g: Generated): Option[ChildGenerated]
	
	final def generateName(g: Generated) = Some(name)
	final def nameClass = Name(name)
	final def generate(matchedName: QName, g: ChildGenerated): Generated = generate(g)
	final def marshalTranslate(generatedName: QName, g: Generated): Option[ChildGenerated] = marshalTranslate(g)
}

private object EmptyElementPattern
{
	val empty = EmptyPattern(())
}
/** A common base class for elements without any content. */
sealed abstract class AbstractEmptyElementPattern[Generated] extends ElementPattern[Generated, Unit]
{
	final def childrenPattern(actualName: QName) = EmptyElementPattern.empty
	final def marshalTranslate(generatedName: QName, g: Generated) = Some(g)
	def traceChildren(writer: Writer, level: Int, reference: ReferenceFunction)
	{
		EmptyElementPattern.empty.embeddedTrace(writer, level, reference)
	}
}
/** An element without any content that matches elements with the name 'name'. */
class NamedEmptyElementPattern[Generated](val name: QName, val value: Generated)
	extends AbstractEmptyElementPattern[Generated]
{
	def nameClass = Name(name)
	def generateName(g: Generated) = Some(name)
	def generate(matchedName: QName, u: Unit) = value
}

/** An element without any content that matches elements with names in the provided 'nameClass'.
* 'generateName' must be implemented to provide the name that should be used when marshalling
* an object.
*/
abstract class EmptyElementPattern[Generated](val nameClass: NameClass) extends AbstractEmptyElementPattern[Generated]
{
	def generate(matchedName: QName): Generated
	final def generate(matchedName: QName, u: Unit): Generated = generate(matchedName)
}

/** A trait that can be mixed in to a pattern to always fail marshalling for that pattern.
* This variant is for patterns that match a general NameClass.
* @see NamedUnmarshalOnly */
trait UnmarshalOnly[Generated]
{
	def marshalTranslate(name: QName, g: Generated) = None
	def generateName(g: Generated) = None
}
/** A trait that can be mixed in to a pattern to always fail marshalling for that pattern. This
* variant is for patterns that match a single name.
* @see UnmarshalOnly */
trait NamedUnmarshalOnly[Generated]
{
	def marshalTranslate(g: Generated) = None
}
/** This common base class is used for patterns that have a fixed childrenPattern independent
* of the matched element name. */
abstract class FixedContent[Generated, ChildGenerated] extends ElementPattern[Generated, ChildGenerated]
{
	/** The pattern for the element's children, independent of the matched name. */
	def childrenPattern: Pattern[ChildGenerated]
	final def childrenPattern(actualName: QName): Pattern[ChildGenerated] = childrenPattern
	def traceChildren(writer: Writer, level: Int, reference: ReferenceFunction)
	{
		childrenPattern.embeddedTrace(writer, level, reference)
	}
}
/** This pattern matches an element with the given 'name' and that has content that matches
* 'childrenPatternA'.  'childrenPatternA' is lazily evaluated and so this pattern may be
* used in a recursive pattern definition.
*/
abstract class NamedElementPattern[Generated, ChildGenerated]
	(val name: QName, childrenPatternA: => Pattern[ChildGenerated])
	extends FixedContent[Generated, ChildGenerated] with NamedElement[Generated, ChildGenerated]
{
	lazy val childrenPattern = childrenPatternA
}
/** This pattern matches an element with name in NameClass 'nameClass' and with content
* that matches 'childrenPatternA'.  'childrenPatternA' is lazily evaluated and so this pattern may be
* used in a recursive pattern definition. */
abstract class GeneralElementPattern[Generated, ChildGenerated]
	(val nameClass: NameClass, childrenPatternA: => Pattern[ChildGenerated])
	extends FixedContent[Generated, ChildGenerated]
{
	lazy val childrenPattern = childrenPatternA
}

/** A common base class for element patterns with a single text value as content. */
sealed abstract class TextElementPattern[Generated] extends FixedContent[Generated, Generated]
{
	def textContent: ValueParser[Generated]
	val childrenPattern = new TextPattern(textContent)
}
/** This pattern matches an element with the given 'name' and with text content matched
* by the given ValueParser. */
class NamedTextElementPattern[Generated](val name: QName, val textContent: ValueParser[Generated])
	extends TextElementPattern[Generated] with NamedElement[Generated, Generated]
{
	def generate(g: Generated) = g
	def marshalTranslate(g: Generated) = Some(g)
}

/** This pattern matches an element with name in NameClass 'nameClass' and with text content matched
* by ValueParser 'textContent'. */
abstract class GeneralTextElementPattern[Generated](val nameClass: NameClass, val textContent: ValueParser[Generated])
	extends TextElementPattern[Generated]
{
	def marshalTranslate(generatedName: QName, g: Generated) = Some(g)
	def generate(matchedName: QName, g: Generated) = g
}

private object ElementContentPattern
{
	def content[Generated, ChildGenerated](matchedName: QName, contentPattern: Pattern[ChildGenerated],
		element: ElementPattern[Generated, ChildGenerated]): Pattern[Generated] =
			contentPattern.ifValid
			{
				new ElementContentPattern[Generated, ChildGenerated](matchedName, contentPattern, element)
			}
}
/**
* A helper pattern that matches the rest of an element after the opening
* angle bracket of a start tag.
*/
private final class ElementContentPattern[Generated, Input]
	(val matchedName: QName, val pattern: Pattern[Input], val element: ElementPattern[Generated, Input])
	extends UnmatchedPattern[Generated] with MarshalInvalid[Generated]
{
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
	
	def derive(node: in.Node) =
	{
		val derived = content(matchedName, pattern.derive(node), element)
		node match
		{
			case end: in.EndTag =>
				pattern.matchEmpty match
				{
					case Some(value) => derived | EmptyPattern(element.generate(matchedName, value))
					case None => derived
				}
			case _ =>
				derived
		}
	}
}