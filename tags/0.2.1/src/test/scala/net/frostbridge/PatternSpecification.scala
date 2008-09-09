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

import org.scalacheck._
import Prop._

object PatternSpecification extends Properties("Pattern")
{
	val gen = new PatternGen
	import gen.arbElement
	specify("matches valid pattern", (test: FTestPattern) =>
	{
		val xml = test.testXML.sample
		(xml.isDefined && xml.get.valid) ==> patternTest(test.pattern, xml.get.fragment)
	})
	private def patternTest[T](pattern: Pattern[T], fragment: Seq[out.Node]): Boolean =
	{
		println("Testing pattern:\n")
		Traceable.trace(pattern)
		import java.io.{StringReader, StringWriter}
		val writer = new StringWriter
		out.StAXOutput.write(fragment, out.StAXOutput.createWriter(writer))
		val xmlString = writer.toString
		Unmarshaller.unmarshalOrError(pattern, in.StAXStream(new StringReader(xmlString))) match
		{
			case Left(errorMessage) =>
			{
				println(errorMessage)
				println("XML was:")
				println(xmlString)
				false
			}
			case Right(matched) =>
			{
				val w = new StringWriter
				Marshaller(matched, pattern, w) match
				{
					case Some(error) =>
					{
						println("Error serializing generated object:\n\n" +matched +"\n\nfor pattern:\n")
						Traceable.trace(pattern)
						println("\nBecause:")
						println(error.getRootCauses.mkString("\n\nand\n\n"))
						false
					}
					case None => true
				}
			}
		}
	}
	
	def runTests
	{
		for((label, Test.Result(status, _, _, _)) <- Test.checkProperties(this))
		{
			status match
			{
				case Test.GenException(e) => e.printStackTrace
				case _ => ()
			}
		}
	}
}
