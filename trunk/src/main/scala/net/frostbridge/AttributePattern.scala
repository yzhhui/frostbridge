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
import util.TList
import java.io.Writer

/**
* A pattern that matches an attribute.  This pattern allows an attribute to be matched
* by name and value.
*/
private sealed abstract class AttributePattern[Generated] extends UnmatchedPattern[Generated] with BasicMarshaller[Generated]
{
	/**
	* Describes the set of allowed names of an attribute matched by this pattern
	*/
	def nameClass: NameClass
	
	def contentDescription: String
	def generate(attributeName: QName, attributeValue: String): Option[Generated]
	def marshalImpl(g: Generated): Option[out.Attribute]
	
	def matchEmpty = None
	
	private[frostbridge] final def deriveImpl(node: in.Node)(implicit o: Optimize): Pattern[Generated] =
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
	
	def marshalImpl(g: Generated, reverseXML: TList[out.Node]): Option[TList[out.Node]] =
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

private final case class GeneralAttributePattern[Generated]
	(nameClass: NameClass, contentDescription: String, generateA: (QName, String) => Option[Generated],
	marshalA: Generated => Option[out.Attribute]) extends AttributePattern[Generated]
{
	def generate(attributeName: QName, attributeValue: String) = generateA(attributeName, attributeValue)
	def marshalImpl(g: Generated) = marshalA(g)
	
	lazy val hash = List(getClass, nameClass, contentDescription, generateA, marshalA).hashCode
}

/** An implementation of AttributePattern that uses a ValueParser
* to determine the value to be matched.
*/
private sealed abstract class BasicAttributePattern[Generated]
	extends AttributePattern[Generated]
{
	def value: ValueParser[Generated]
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
private final case class SimpleAttributePattern[Generated](nameClass: Name, value: ValueParser[Generated])
	extends BasicAttributePattern[Generated]
{
	def generateName(g: Generated) = Some(nameClass.name)
	lazy val hash = List(getClass, nameClass, value).hashCode
}

private final case class AdvancedAttributePattern[Generated]
	(nameClass: NameClass, value: ValueParser[Generated], generateNameA: Generated => Option[QName])
	extends BasicAttributePattern[Generated]
{
	def generateName(g: Generated) = generateNameA(g)
	lazy val hash = List(getClass, nameClass, value, generateNameA).hashCode
}


private[frostbridge] trait AttributePatternFactory
{
	def attribute[Generated](name: Name, value: ValueParser[Generated])(implicit o: Optimize): Pattern[Generated] =
		o.intern(SimpleAttributePattern[Generated](name, value))
	
	def attribute[Generated](nameClass: NameClass, value: ValueParser[Generated],
		generateNameA: Generated => Option[QName])(implicit o: Optimize): Pattern[Generated] =
			o.intern(AdvancedAttributePattern[Generated](nameClass, value, generateNameA))
		
	def generalAttribute[Generated](nameClass: NameClass, contentDescription: String,
		generate: (QName, String) => Option[Generated],
		marshal: Generated => Option[out.Attribute])(implicit o: Optimize): Pattern[Generated] =
			o.intern(GeneralAttributePattern[Generated](nameClass, contentDescription, generate, marshal))
}
