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

import javax.xml.stream.XMLStreamWriter

/** The Marshaller object provides a thin (but important) wrapper around the Pattern.marshal
* function.
*/
object Marshaller
{
	import java.io.{OutputStream, Writer}
	def apply[Generated](value: Generated, pattern: Pattern[Generated], output: Writer): Option[MarshalException[Generated]] =
		apply(value, pattern, out.StAXOutput.createWriter(output))
	def apply[Generated](value: Generated, pattern: Pattern[Generated], output: OutputStream): Option[MarshalException[Generated]] =
		apply(value, pattern, out.StAXOutput.createWriter(output))
	
	def apply[Generated](value: Generated, pattern: Pattern[Generated], output: XMLStreamWriter): Option[MarshalException[Generated]] =
	{
		pattern.marshal(value, Nil) match
		{
			case Right(nodeList) => out.StAXOutput.write(nodeList.reverse, output); None
			case Left(error) => Some(error)
		}
	}
}