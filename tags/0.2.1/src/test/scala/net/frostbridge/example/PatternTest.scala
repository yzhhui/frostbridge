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

/** This class provides basic code for evaluating a pattern.  It unmarshals
* a document according to the pattern provided in its constructor and then
* marshals the document to standard output or to an in-memory buffer that
* is discarded after marshalling.*/
class PatternTest[Generated](val pattern: Pattern[Generated])
{
	/** Calls 'test', using the source document in the File for the given 'filename'
	*  and the default value of true for 'echoResult'.
	* @see test */
	def apply(filename: String): Unit = apply(filename, true)
	/** Calls 'test', using the source document in the File for the given 'filename'
	*  and the given value for 'echoResult'.
	* @see test */
	def apply(filename: String, echoResult: Boolean): Unit =
	{
		val file = new File(filename)
		if(file.exists)
			test(in.StAXStream(file), echoResult)
		else
			println("File does not exist.")
	}
	/** Calls 'test', using the source document at the given URL and the default
	* value of true for 'echoResult'.
	* @see test */
	def apply(url: URL): Unit = apply(url, true)
	/** Calls 'test', using the source document at the given URL and the given
	* value of 'echoResult'.
	* @see test */
	def apply(url: URL, echoResult: Boolean): Unit = test(in.StAXStream(url), echoResult)
	
	/** Unmarshals the document provided by 'source' to an object, printing the
	* unmarshal execution time and, if 'echoResult' is true, the toString representation
	* of the object.
	* Then, it marshals the object to standard output if 'echoResult' is true, otherwise
	* it marshals the object to an in-memory buffer that is discarded after marshalling.
	* The marshal execution time is then printed.*/
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
	
	def generated(g: Generated, echoResult: Boolean): Unit =
	{
		if(echoResult)
		{
			println()
			println(g.toString)
			println('\n')
		}
	}
}