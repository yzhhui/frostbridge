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

import java.io.Writer
import Traceable.{basicTrace, ReferenceFunction}
import PatternImpl.translateNotAllowed
import data.ValueParser

/**
* A pattern that matches text and generates a value using a ValueParser.
*/
trait TextPattern[Generated] extends UnmatchedPattern[Generated] with BasicMarshaller[Generated]
{
	def content: ValueParser[Generated]
	lazy val matchEmpty = content.generate("")
	
	def derive(node: in.Node) =
	{
		node match
		{
			case text: in.Text =>
			{
				content.generate(text.value) match
				{
					case Some(value) => EmptyPattern(value)
					case None => NotAllowedPattern
				}
			}
			case close: in.Close => this
			case _ => NotAllowedPattern
		}
	}
	
	protected def marshalImpl(g: Generated, reverseXML: List[out.Node]) =
	{
		for(string <- content.marshalToString(g)) yield
			out.Text(string) :: reverseXML
	}
		
	def trace(writer: Writer, level: Int, reference: ReferenceFunction) = basicTrace(writer, level, "text")
	
	def nextPossiblePatterns = List(this)
	def description = content.dataDescription
}

class BasicTextPattern[Generated](val content: ValueParser[Generated]) extends TextPattern[Generated]