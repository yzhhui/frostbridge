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

import PatternImpl._
import Traceable._
import PatternFactory._
import data.ValueParser
import java.io.Writer

/**
* A pattern that matches an attribute.  This pattern allows an attribute to be matched
* by name and value.
*/
private sealed trait AttributePattern[Generated] extends UnmatchedPattern[Generated]
	with BasicMarshaller[Generated]
{
	/**
	* Describes the set of allowed names of an attribute matched by this pattern
	*/
	def nameClass: NameClass
	
	def contentDescription: String
	def generate(attributeName: QName, attributeValue: String): Option[Generated]
	def marshalImpl(g: Generated): Option[out.Attribute]
	
	def matchEmpty = None
	
	def derive(node: in.Node): Pattern[Generated] =
	{
		node match
		{
			case attribute: in.Attribute =>
			{
				if(nameClass.matches(attribute.name))
					generate(attribute.name, attribute.value).map(emptyPattern(_)).getOrElse(NotAllowedPattern)
				else
					NotAllowedPattern
			}
			case _ => NotAllowedPattern
		}
	}
	
	def marshalImpl(g: Generated, reverseXML: List[out.Node]): Option[List[out.Node]] =
		for(attribute <- marshalImpl(g)) yield
			attribute :: reverseXML
	
	def description = "attribute " + nameClass.description + "={" + contentDescription + "}"
	
	def nextPossiblePatterns = List(this)
	def trace(writer: Writer, level: Int, reference: ReferenceFunction)
	{
		writeLevel(writer, level)
		writer.write("attribute ")
		writer.write(nameClass.description)
		writer.write(" { ")
		traceContent(writer)
		writer.write(" }\n")
	}
	
	protected def traceContent(writer: Writer) = writer.write(contentDescription)
}

/** An implementation of AttributePattern that uses a ValueParser
* to determine the value to be matched.
*/
private sealed abstract class BasicAttributePattern[Generated]
	(val nameClass: NameClass, value: ValueParser[Generated]) extends AttributePattern[Generated]
{
	def generateName(g: Generated): Option[QName]
	
	def contentDescription = value.dataDescription
	
	def generate(name: QName, valueString: String) = value.generate(valueString)
	
	def marshalImpl(g: Generated): Option[out.Attribute] =
	{
		for(name <- generateName(g);
			value <- value.marshalToString(g))
		yield
			out.Attribute(name, value)
	}
}
private final class SimpleAttributePattern[Generated](name: Name, value: ValueParser[Generated])
	extends BasicAttributePattern[Generated](name, value)
{
	def generateName(g: Generated) = Some(name.name)
}

private[frostbridge] trait AttributePatternFactory
{
	def attribute[Generated](name: Name, value: ValueParser[Generated]): Pattern[Generated] =
		new SimpleAttributePattern[Generated](name, value)
	
	def attribute[Generated](nameClass: NameClass, value: ValueParser[Generated],
		generateNameA: Generated => Option[QName]): Pattern[Generated] =
			new BasicAttributePattern[Generated](nameClass, value)
			{
				def generateName(g: Generated) = generateNameA(g)
			}
		
	def generalAttribute[Generated](nameClassA: NameClass, contentDescriptionA: String,
		generateA: (QName, String) => Option[Generated],
		marshalA: Generated => Option[out.Attribute]) =
			new AttributePattern[Generated]
			{
				val nameClass = nameClassA
				val contentDescription = contentDescriptionA
				def generate(attributeName: QName, attributeValue: String) = generateA(attributeName, attributeValue)
				def marshalImpl(g: Generated) = marshalA(g)
			}
}
