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

class IgnoreAnyInNameClass(nameClass: NameClass)(implicit o: Optimize)
{
	val unit2None = (u: Unit) => None
	
	val anyValue: ValueParser[Unit] =
		new ValueParser[Unit]
		{
			def generate(value: String) = if(value.length == 0) None else Some(())
			def marshalToString(value: Unit) = None
			def dataDescription = "any value"
		}
		
	val anyElement: Pattern[Unit] =
		recursiveGeneralElement(nameClass, lazyAnyPatterns,
		(q: QName, u: Unit) => (),
		(q: QName, u: Unit) => None,
		unit2None)
	
	val anyAttribute: Pattern[Unit] = attribute(nameClass, anyValue, unit2None)
	val anyText: Pattern[Unit] = textPattern(anyValue)
	val anyComment: Pattern[Unit] = commentPattern(anyValue)
	val anyProcessingInstruction: Pattern[Unit] =
		generalProcessingInstructionPattern[Unit]("any processing instruction", (pi: in.ProcessingInstruction) => Some(pi), unit2None)
	
	val any: Pattern[Unit] = anyElement | anyAttribute | anyText | anyComment | anyProcessingInstruction
	val anyPatterns: Pattern[Unit] = translate(any*, UnitTranslator)
	def lazyAnyPatterns(u: Unit) = anyPatterns
	
	private case object UnitTranslator extends Translator[Unit, Seq[Unit]]
	{
		def process(u: Seq[Unit]) = ()
		def unprocess(u: Unit) = None
	}
}
class IgnoreAnyInNamespace(ns: String)(implicit o: Optimize) extends IgnoreAnyInNameClass(NsName(ns))(o)
class IgnoreAny(implicit o: Optimize) extends IgnoreAnyInNameClass(AnyName)(o)


sealed abstract class AbstractPreserveAny(nameClass: NameClass)(implicit o: Optimize)
{
	import out._
	protected def lazyAnyPatterns(u: Unit): Pattern[Seq[Node]]
	
	val anyElement: Pattern[Element] =
		recursiveGeneralElement(nameClass, 
			lazyAnyPatterns,
			(actualName: QName, childValue: Seq[Node]) => Element(actualName, childValue),
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
			def generate(value: String) = if(value.length > 0) Some(Text(value)) else None
			def marshalToString(text: Text) = Some(text.text)
			def dataDescription = "any text"
		}
		textPattern[Text](textValue)
	}
}
class PreserveAnyInNameClass(nameClass: NameClass)(implicit o: Optimize) extends AbstractPreserveAny(nameClass)(o)
{
	import out._
	val any: Pattern[Node] =
	{
		val mixed = anyElement |+| anyAttribute |+| anyText
		val translator = new Translator[Node, mixed.GeneratedType]
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
		translate(mixed, translator)
	}
	
	val anyPatterns: Pattern[Seq[Node]] = any*
	def lazyAnyPatterns(u: Unit) = anyPatterns
}

class PreserveAnyInNamespace(ns: String)(implicit o: Optimize) extends PreserveAnyInNameClass(NsName(ns))(o)
class PreserveAny(implicit o: Optimize) extends PreserveAnyInNameClass(AnyName)(o)

class FullPreserveAnyInNameClass(nameClass: NameClass)(implicit o: Optimize) extends AbstractPreserveAny(nameClass)(o)
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
		commentPattern[Comment](commentValue)
	}
	val anyProcessingInstruction: Pattern[ProcessingInstruction] =
		generalProcessingInstructionPattern[ProcessingInstruction]("any processing instruction",
			(pi: in.ProcessingInstruction) => Some(out.ProcessingInstruction(pi.target, pi.data)),
			(pi: ProcessingInstruction) => Some(pi))
		
	val any: Pattern[Node] =
	{
		val mixed = anyElement |+| anyAttribute |+| anyText |+| anyComment |+| anyProcessingInstruction
		val translator = new Translator[Node, mixed.GeneratedType]
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
		translate(mixed, translator)
	}
	
	val anyPatterns: Pattern[Seq[Node]] = any*
	def lazyAnyPatterns(u: Unit) = anyPatterns
}

class FullPreserveAnyInNamespace(ns: String)(implicit o: Optimize) extends FullPreserveAnyInNameClass(NsName(ns))(o)
class FullPreserveAny(implicit o: Optimize) extends FullPreserveAnyInNameClass(AnyName)(o)


class PreserveAnyInNameClass2(nameClass: NameClass)(implicit o: Optimize)
{
	import out._
	
	val anyElement: Pattern[Node] =
		recursiveGeneralElement[Node, Seq[Node]](nameClass, 
			lazyAnyPatterns,
			(actualName: QName, childValue: Seq[Node]) => Element(actualName, childValue): Node,
			(generatedName: QName, n: Node) => n match { case e: Element => Some(e.content); case _ => None },
			(n: Node) => n match { case e: Element => Some(e.name); case _ => None } )
	
	val anyAttribute: Pattern[Node] = 
		generalAttribute(nameClass, 
		"any", 
		(name: QName, value: String) => Some(Attribute(name, value): Node),
		(n: Node) => n match { case a: Attribute => Some(a); case _ => None })
		
	val anyText: Pattern[Node] =
	{
		val textValue = new ValueParser[Node]
		{
			def generate(value: String) = if(value.length > 0) Some(Text(value): Node) else None
			def marshalToString(node: Node) = node match { case t: Text => Some(t.text); case _ => None }
			def dataDescription = "any text"
		}
		textPattern[Node](textValue)
	}
	
	val any: Pattern[Node] = anyElement | anyAttribute | anyText
	val anyPatterns: Pattern[Seq[Node]] = any*
	def lazyAnyPatterns(u: Unit) = anyPatterns
}

class PreserveAnyInNamespace2(ns: String)(implicit o: Optimize) extends PreserveAnyInNameClass2(NsName(ns))(o)
class PreserveAny2(implicit o: Optimize) extends PreserveAnyInNameClass2(AnyName)(o)