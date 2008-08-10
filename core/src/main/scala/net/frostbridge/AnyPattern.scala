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

import data.ValueParser
import PatternFactory._

class IgnoreAnyInNameClass(nameClass: NameClass)
{
	val unit2None = (u: Unit) => None
	val unitList2None = (a: List[Unit]) => ()
	
	val anyValue: ValueParser[Unit] =
		new ValueParser[Unit]
		{
			def generate(value: String) = Some(())
			def marshalToString(value: Unit) = None
			def dataDescription = "any value"
		}
		
	val anyElement: Pattern[Unit] =
		generalElement(nameClass, anyPatterns,
		(q: QName, u: Unit) => (),
		(q: QName, u: Unit) => None,
		unit2None)
	
	val anyAttribute: Pattern[Unit] = attribute(nameClass, anyValue, unit2None)
	val anyText: Pattern[Unit] = textPattern(anyValue)
	val anyComment: Pattern[Unit] = commentPattern(anyValue)
	val anyProcessingInstruction: Pattern[Unit] =
		generalProcessingInstructionPattern[Unit]("any processing instruction", (pi: in.ProcessingInstruction) => Some(pi), unit2None)
	
		
	val any: Pattern[Unit] = anyElement | anyAttribute | anyText | anyComment | anyProcessingInstruction
	val anyPatterns: Pattern[Unit] = translate(any*, unitList2None, unit2None)
}
class IgnoreAnyInNamespace(ns: String) extends IgnoreAnyInNameClass(NsName(ns))
object IgnoreAny extends IgnoreAnyInNameClass(AnyName)


class PreserveAnyInNameClass(nameClass: NameClass)
{
	import out._
	val anyElement: Pattern[Element] =
		generalElement(nameClass, 
			anyPatterns,
			(actualName: QName, childValue: List[Node]) => Element(actualName, childValue),
			(generatedName: QName, e: Element) => Some(e.content),
			(e: Element) => Some(e.name))
	
	val anyAttribute: Pattern[Attribute] = 
		generalAttribute(nameClass, 
		"any", 
		(name: QName, value: String) => Some(Attribute(name, value)),
		(a: Attribute) => Some(a))
		
	val anyText: Pattern[Text] =
	{
		val textValue = new ValueParser[Text]
		{
			def generate(value: String) = Some(Text(value))
			def marshalToString(text: Text) = Some(text.text)
			def dataDescription = "any text"
		}
		textPattern[Text](textValue)
	}
	val anyComment: Pattern[Comment] =
	{
		val commentValue = new ValueParser[Comment]
		{
			def generate(value: String) = Some(Comment(value))
			def marshalToString(comment: Comment) = Some(comment.text)
			def dataDescription = "any text"
		}
		commentPattern[Comment](commentValue)
	}
	val anyProcessingInstruction: Pattern[ProcessingInstruction] =
		generalProcessingInstructionPattern[ProcessingInstruction]("any processing instruction",
			(pi: in.ProcessingInstruction) => Some(out.ProcessingInstruction(pi.target, pi.data)),
			(pi: ProcessingInstruction) => Some(pi))
		
	val any: Pattern[Node] =
	{
		val mixed = anyElement |+| anyAttribute |+| anyText |+| anyComment |+| anyProcessingInstruction
		def generate(m: mixed.GeneratedType): Node =
		{
			val element |+| attribute |+| text |+| comment |+| processingInstruction = m
			(element :: attribute :: text :: comment :: processingInstruction :: Nil).flatMap(_.toList).head
		}
		def unprocess(g: Node) =
			Some(g match
			{
				case e: Element => Left(Left(Left(Left(e))))
				case a: Attribute => Left(Left(Left(Right(a))))
				case t: Text => Left(Left(Right(t)))
				case c: Comment => Left(Right(c))
				case pi: ProcessingInstruction => Right(pi)
			})
		translate(mixed, generate, unprocess)
	}
	
	val anyPatterns: Pattern[List[Node]] = any*
}

class PreserveAnyInNamespace(ns: String) extends PreserveAnyInNameClass(NsName(ns))
object PreserveAny extends PreserveAnyInNameClass(AnyName)