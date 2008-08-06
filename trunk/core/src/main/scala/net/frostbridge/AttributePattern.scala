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
import data.ValueParser
import java.io.Writer

/**
* A pattern that matches an attribute.  This pattern allows an attribute to be matched
* by name and content.
* The implementation of ValueParser describes the content to be matched.
*/
trait AttributePattern[Generated] extends UnmatchedPattern[Generated]
	with NameMarshaller[Generated] with BasicMarshaller[Generated]
{
	/**
	* Describes the set of allowed names of an attribute matched by this pattern
	*/
	def nameClass: NameClass
	
	def matchEmpty = None
	
	def value: ValueParser[Generated]
	
	def derive(node: in.Node): Pattern[Generated] =
	{
		node match
		{
			case attribute: in.Attribute =>
			{
				if(nameClass.matches(attribute.name))
				{
					value.generate(attribute.value) match
					{
						case Some(value) => EmptyPattern(value)
						case None => NotAllowedPattern
					}
				}
				else
					NotAllowedPattern
			}
			case _ => NotAllowedPattern
		}
	}
	protected def marshalImpl(g: Generated, reverseXML: List[out.Node]): Option[List[out.Node]] =
	{
		for(name <- generateName(g);
			value <- value.marshalToString(g))
		yield
			out.Attribute(name, value) :: reverseXML
	}
	
	def name = nameClass.description
	def description = "attribute " + nameClass.description + "={" + value.dataDescription + "}"
	
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
	
	protected def traceContent(writer: Writer) = ()
}

class BasicAttributePattern[Generated](val nameClass: NameClass, val value: ValueParser[Generated])
	extends AttributePattern[Generated]