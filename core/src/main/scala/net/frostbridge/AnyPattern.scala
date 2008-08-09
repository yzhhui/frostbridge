package net.frostbridge

import data.ValueParser

class IgnoreAnyInNameClass(nameClass: NameClass)
{
	val unit2Unit = (u: Unit) => ()
	val unit2None = (u: Unit) => None
	val unitList2None = (a: List[Unit]) => ()
	
	val anyValue: ValueParser[Unit] =
		new ValueParser[Unit]
		{
			def generate(value: String) = Some(())
			def marshalToString(value: Unit) = None
			def dataDescription = "any value"
		}
		
	val anyElement: ElementPattern[Unit, Unit] = new BasicElementPattern(nameClass, anyPatterns, unit2Unit, unit2None)
	val anyAttribute: AttributePattern[Unit] = new SimpleAttributePattern(nameClass, anyValue)
	val anyText: TextPattern[Unit] = new BasicTextPattern(anyValue)
	val anyComment: CommentPattern[Unit] = new BasicCommentPattern(anyValue)
	val anyProcessingInstruction: ProcessingInstructionPattern[Unit] =
		new ProcessingInstructionPattern[Unit]
		{
			def generate(target: String, data: String) = Some(())
			def getTargetAndValue(u: Unit) = None
			def description = "any processing instruction"
		}
	
		
	val any: Pattern[Unit] = anyElement | anyAttribute | anyText | anyComment | anyProcessingInstruction
	val anyPatterns: Pattern[Unit] = TranslatingPattern.translate(any*, unitList2None, unit2None)
}
class IgnoreAnyInNamespace(ns: String) extends IgnoreAnyInNameClass(NsName(ns))
object IgnoreAny extends IgnoreAnyInNameClass(AnyName)


class PreserveAnyInNameClass(nameClass: NameClass)
{ outer =>
	import out._
	val anyElement: ElementPattern[Element, List[Node]] =
		new ElementPattern[Element, List[Node]]
		{
			def nameClass = outer.nameClass
			def childrenPattern = anyPatterns
			def generate(actualName: QName, childValue: List[Node]) = Element(actualName, childValue)
			def marshalTranslate(name: QName, e: Element) = Some(e.content)
			override def generateName(e: Element) = Some(e.name)
		}
	
	val anyAttribute: AttributePattern[Attribute] =
		new AttributePattern[Attribute]
		{
			def nameClass = outer.nameClass
			override def contentDescription = "any"
			def marshalImpl(a: Attribute) = Some(a)
			def generate(attributeName: QName, attributeValue: String) = Some(Attribute(attributeName, attributeValue))
		}
		
	val anyText: TextPattern[Text] =
	{
		val textValue = new ValueParser[Text]
		{
			def generate(value: String) = Some(Text(value))
			def marshalToString(text: Text) = Some(text.text)
			def dataDescription = "any text"
		}
		new BasicTextPattern[Text](textValue)
	}
	val anyComment: CommentPattern[Comment] =
	{
		val commentValue = new ValueParser[Comment]
		{
			def generate(value: String) = Some(Comment(value))
			def marshalToString(comment: Comment) = Some(comment.text)
			def dataDescription = "any text"
		}
		new BasicCommentPattern[Comment](commentValue)
	}
	val anyProcessingInstruction: ProcessingInstructionPattern[ProcessingInstruction] =
		new ProcessingInstructionPattern[ProcessingInstruction]
		{
			def generate(target: String, data: String) = Some(ProcessingInstruction(target, data))
			def getTargetAndValue(pi: ProcessingInstruction) = Some((pi.target, pi.data))
			def description = "any processing instruction"
		}
		
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
		TranslatingPattern.translate(mixed, generate, unprocess)
	}
	
	val anyPatterns: Pattern[List[Node]] = any*
	
	
}

class PreserveAnyInNamespace(ns: String) extends PreserveAnyInNameClass(NsName(ns))
object PreserveAny extends PreserveAnyInNameClass(AnyName)