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
package net.frostbridge.example

import net.frostbridge._

import java.net.URL
import java.io.{File, OutputStreamWriter, StringWriter}

class PatternTest[Generated](val pattern: Pattern[Generated])
{
	def generated(g: Generated, echoResult: Boolean): Unit =
	{
		if(echoResult)
		{
			println()
			println(g.toString)
			println('\n')
		}
	}
	
	def apply(filename: String): Unit = apply(filename, true)
	def apply(filename: String, echoResult: Boolean): Unit =
	{
		val file = new File(filename)
		if(file.exists)
			test(in.StAXStream(file), echoResult)
		else
			println("File does not exist.")
	}
	def apply(url: URL): Unit = apply(url, true)
	def apply(url: URL, echoResult: Boolean): Unit = test(in.StAXStream(url), echoResult)
	
	def test(source: in.XMLStream, echoResult: Boolean)
	{
		val unmarshalStartTime = System.currentTimeMillis
		Unmarshaller.unmarshalOrError(pattern, source) match
		{
			case Right(parsed) =>
			{
				val unmarshalEndTime = System.currentTimeMillis
				val unmarshalTime = (unmarshalEndTime - unmarshalStartTime)/1000.0
				println("\nSuccessfully unmarshalled document in " + unmarshalTime + " s:")
				generated(parsed, echoResult)
				
				val marshalStartTime = System.currentTimeMillis
				val writer = if(echoResult) new OutputStreamWriter(System.out) else new StringWriter
				Marshaller(parsed, pattern, writer) match
				{
					case None =>
					{
						val marshalEndTime = System.currentTimeMillis
						val marshalTime = (marshalEndTime - marshalStartTime)/1000.0
						println("\n\nMarshalled successfully in " + marshalTime +" s.")
					}
					case Some(error) => println(error)
				}
			}
			case Left(e) => println(e.toString)
		}
	}
}