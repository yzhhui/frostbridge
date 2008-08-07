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
import Traceable._
import data.ValueParser
import java.io.Writer

/**
* A pattern that matches an element based on its name, attributes, child elements, and text content.
*/
trait ElementPattern[Generated, ChildGenerated] extends UnmatchedPattern[Generated]
	with ReferencedTraceable with NameMarshaller[Generated] with MarshalErrorTranslator[Generated]
{
	/**
	* Describes the set of allowed names of an element matched by this pattern.
	*/
	def nameClass: NameClass
	
	def matchEmpty = None
	
	/**
	* The child pattern of this element.  This pattern will match attributes, child elements, and/or
	* text content.  Use a compound pattern for multiple children and EmptyPattern for no content.
	*/
	val childrenPattern: Pattern[ChildGenerated]
	
	/**
	* A function that processes the result of the child pattern.
	*/
	def generate(matchedName: QName, childResult: ChildGenerated): Generated
	
	final def derive(node: in.Node): Pattern[Generated] =
	{
		node match
		{
			case open: in.Open =>
			{
				val actualName = open.elementName
				if(nameClass.matches(actualName))
					content(childrenPattern, actualName, generate)
				else
					NotAllowedPattern
			}
			case close: in.Close => this
			case _ => NotAllowedPattern
		}
	}
	
	def orError[B](g: Generated, value: Option[B]): Either.RightProjection[RootMarshalException[Generated],B] =
		value.toRight(RootMarshalException(g, this)).right
	
	def marshal(g: Generated, reverseXML: List[out.Node]) =
	{
		translateMarshalError(g)
		{
			for(name <- orError(g, generateName(g));
				childValue <- orError(g, marshalTranslate(name, g));
				content <- childrenPattern.marshal(childValue, Nil).right)
			yield
				out.Element(name)(content.reverse) :: reverseXML
		}
	}
	def marshalTranslate(generatedName: QName, g: Generated): Option[ChildGenerated]
	
	def nextPossiblePatterns = List(this)
	def description = "element '" + nameClass.description + "'"
	def name = nameClass.description
	def trace(writer: Writer, level: Int, reference: ReferenceFunction) =
	{
		basicTrace(writer, level, "element " + nameClass.description)
		basicTrace(writer, level, "{")
		childrenPattern.embeddedTrace(writer, level+1, reference)
		basicTrace(writer, level, "}")
	}
}

class BasicElementPattern[Generated, ChildGenerated]
	(val nameClass: NameClass, val childrenPattern: Pattern[ChildGenerated],
	val generateA: ChildGenerated => Generated,
	val marshalTranslateA: Generated => Option[ChildGenerated]) extends ElementPattern[Generated, ChildGenerated]
{
	def generate(actualName: QName, childValue: ChildGenerated): Generated = generateA(childValue)
	def marshalTranslate(generatedName: QName, g: Generated) = marshalTranslateA(g)
}

class TextElementPattern[Generated](val nameClass: NameClass, textContent: ValueParser[Generated])
	extends ElementPattern[Generated, Generated]
{
	val childrenPattern = new BasicTextPattern(textContent)
	def generate(matchedName: QName, textValue: Generated) = textValue
	def marshalTranslate(generatedName: QName, g: Generated) = Some(g)
}

/**
* A pattern that matches an element without any attributes, child elements, or text.
* The generated object is the value passed to the constructor.
*/
class EmptyElementPattern[Generated](val nameClass: NameClass, value: QName => Generated) extends ElementPattern[Generated, Unit]
{
	val childrenPattern = EmptyPattern(())
	def generate(matchedName: QName, u: Unit): Generated = value(matchedName)
	def marshalTranslate(generatedName: QName, g: Generated) = Some(())
}

object ElementContentPattern
{
	def content[Generated, ChildGenerated](patternA: Pattern[ChildGenerated], matchedNameA: QName,
		generateA: (QName, ChildGenerated) => Generated): Pattern[Generated] =
	{
		patternA.ifValid
		{
			new ElementContentPattern[Generated, ChildGenerated]
			{
				def generate = generateA
				def matchedName = matchedNameA
				def pattern = patternA
			}
		}
	}
}
/**
* A helper pattern that matches the rest of an element after the opening
* angle bracket of a start tag.
*/
private sealed trait ElementContentPattern[Generated, Input] extends UnmatchedPattern[Generated] with MarshalInvalid[Generated]
{
	def matchedName: QName
	def matchEmpty = None
	def pattern: Pattern[Input]
	
	def generate: (QName, Input) => Generated
	
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
		node match
		{
			case end: in.EndTag =>
				val derived = content(pattern.derive(node), matchedName, generate)
				pattern.matchEmpty match
				{
					case Some(value) => derived | EmptyPattern(generate(matchedName, value))
					case None => derived
				}
			case _ =>
				content(pattern.derive(node), matchedName, generate)
		}
	}
}