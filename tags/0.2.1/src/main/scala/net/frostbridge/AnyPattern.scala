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
	val anyValue: ValueParser[Unit] =
		new ValueParser[Unit]
		{
			def generate(value: String) = if(value.length == 0) None else Some(())
			def marshalToString(value: Unit) = None
			def dataDescription = "any value"
		}
		
	val anyElement: Pattern[Unit] =
		new GeneralElementPattern[Unit, Unit](nameClass, anyPatterns) with UnmarshalOnly[Unit]
		{
			def generate(q: QName, u: Unit) = ()
		}
	
	val anyAttribute: Pattern[Unit] =
		new GeneralAttributePattern(nameClass, anyValue) { def generateName(u: Unit) = None }
	val anyText: Pattern[Unit] = text(anyValue)
	val anyComment: Pattern[Unit] = comment(anyValue)
	val anyProcessingInstruction: Pattern[Unit] =
		new ProcessingInstructionPattern[Unit]
		{
			def description = "any processing instruction"
			def generate(pi: in.ProcessingInstruction) = Some(pi)
			def getTargetAndValue(u: Unit) = None
		}
	
	val any: Pattern[Unit] = anyElement | anyAttribute | anyText | anyComment | anyProcessingInstruction
	val anyPatterns: Pattern[Unit] = (any*) >>= UnitMapping
	
	private case object UnitMapping extends Mapping[Unit, Seq[Unit]]
	{
		def process(u: Seq[Unit]) = ()
		def unprocess(u: Unit) = None
	}
}
class IgnoreAnyInNamespace(ns: String) extends IgnoreAnyInNameClass(NsName(ns))
object IgnoreAny extends IgnoreAnyInNameClass(AnyName)


sealed abstract class AbstractPreserveAny(allowedNames: NameClass)
{
	import out._
	protected def anyPatterns: Pattern[Seq[Node]]
	
	val anyElement: Pattern[Element] =
		new GeneralElementPattern[Element, Seq[Node]](allowedNames, anyPatterns)
		{
			def generate(actualName: QName, childValue: Seq[Node]) = Element(actualName, childValue)
			def marshalTranslate(generatedName: QName, e: Element) = Some(e.content)
			def generateName(e: Element) = Some(e.name)
		}
	
	val anyAttribute: Pattern[Attribute] =
		new AttributePattern[Attribute]
		{
			def nameClass = allowedNames
			def contentDescription = "any"
			def generate(name: QName, value: String) = Some(Attribute(name, value))
			def marshalImpl(a: Attribute) = Some(a)
		}
		
	val anyText: Pattern[Text] =
	{
		val textValue = new ValueParser[Text]
		{
			def generate(value: String) = if(value.length > 0) Some(Text(value)) else None
			def marshalToString(text: Text) = Some(text.text)
			def dataDescription = "any text"
		}
		text(textValue)
	}
}
class PreserveAnyInNameClass(nameClass: NameClass) extends AbstractPreserveAny(nameClass)
{
	import out._
	val any: Pattern[Node] =
	{
		val mixed = anyElement |+| anyAttribute |+| anyText
		val mapping = new Mapping[Node, mixed.GeneratedType]
		{
			def process(m: mixed.GeneratedType): Node =
			{
				val element |+| attribute |+| text = m
				(element :: attribute :: text :: Nil).flatMap(_.toList).head
			}
			def unprocess(g: Node) =
				g match
				{
					case e: Element => Some(Left(Left(e)))
					case a: Attribute => Some(Left(Right(a)))
					case t: Text => Some(Right(t))
					case _ => None
				}
		}
		mixed >>= mapping
	}
	
	val anyPatterns: Pattern[Seq[Node]] = any*
}

class PreserveAnyInNamespace(ns: String) extends PreserveAnyInNameClass(NsName(ns))
object PreserveAny extends PreserveAnyInNameClass(AnyName)

class FullPreserveAnyInNameClass(nameClass: NameClass) extends AbstractPreserveAny(nameClass)
{
	import out._
	
	val anyComment: Pattern[Comment] =
	{
		val commentValue = new ValueParser[Comment]
		{
			def generate(value: String) = Some(Comment(value))
			def marshalToString(comment: Comment) = Some(comment.text)
			def dataDescription = "any text"
		}
		comment(commentValue)
	}
	val anyProcessingInstruction: Pattern[ProcessingInstruction] =
		new ProcessingInstructionPattern[ProcessingInstruction]
		{
			def description = "any processing instruction"
			def generate(pi: in.ProcessingInstruction) = Some(out.ProcessingInstruction(pi.target, pi.data))
			def getTargetAndValue(pi: ProcessingInstruction) = Some(pi)
		}
		
	val any: Pattern[Node] =
	{
		val mixed = anyElement |+| anyAttribute |+| anyText |+| anyComment |+| anyProcessingInstruction
		val mapping = new Mapping[Node, mixed.GeneratedType]
		{
			def process(m: mixed.GeneratedType): Node =
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
		}
		mixed >>= mapping
	}
	
	val anyPatterns: Pattern[Seq[Node]] = any*
}

class FullPreserveAnyInNamespace(ns: String) extends FullPreserveAnyInNameClass(NsName(ns))
object FullPreserveAny extends FullPreserveAnyInNameClass(AnyName)