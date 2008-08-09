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
trait TextPattern[Generated] extends ContentPattern[Generated]
{
	lazy val matchEmpty = content.generate("")
	def content: ValueParser[Generated]
	protected def nodeFromString(string: String) = out.Text(string)
	
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
	def description = content.dataDescription
}

class BasicTextPattern[Generated](val content: ValueParser[Generated]) extends TextPattern[Generated]


/** A pattern that has some single content value (Text or a Comment) */
trait ContentPattern[Generated] extends UnmatchedPattern[Generated] with BasicMarshaller[Generated]
{
	def content: ValueParser[Generated]
	
	protected def marshalImpl(g: Generated, reverseXML: List[out.Node]) =
	{
		for(string <- content.marshalToString(g)) yield
			nodeFromString(string) :: reverseXML
	}
	
	protected def nodeFromString(string: String): out.Node
		
	def trace(writer: Writer, level: Int, reference: ReferenceFunction) = basicTrace(writer, level, description)
	
	def nextPossiblePatterns = List(this)
}

/**
* A pattern that matches a comment and generates a value from the comment content using a ValueParser.
*/
trait CommentPattern[Generated] extends ContentPattern[Generated]
{
	lazy val matchEmpty = None
	def derive(node: in.Node) =
	{
		node match
		{
			case comment: in.Comment =>
			{
				content.generate(comment.content) match
				{
					case Some(value) => EmptyPattern(value)
					case None => NotAllowedPattern
				}
			}
			case close: in.Close => this
			case _ => NotAllowedPattern
		}
	}
	
	def description = "comment { " + content.dataDescription + " }"
	protected def nodeFromString(string: String) = out.Comment(string)
}

class BasicCommentPattern[Generated](val content: ValueParser[Generated]) extends CommentPattern[Generated]

/** A pattern that matches a processing instruction and generates a value from its 
* target and data. */
trait ProcessingInstructionPattern[Generated] extends UnmatchedPattern[Generated] with BasicMarshaller[Generated]
{
	def generate(target: String, data: String): Option[Generated]
	def getTargetAndValue(g: Generated): Option[(String, String)]
	
	lazy val matchEmpty = None
	
	def derive(node: in.Node): Pattern[Generated] =
	{
		node match
		{
			case pi: in.ProcessingInstruction => generate(pi.target, pi.data).map(EmptyPattern(_)).getOrElse(NotAllowedPattern)
			case close: in.Close => this
			case _ => NotAllowedPattern
		}
	}
	def marshalImpl(g: Generated, reverseXML: List[out.Node]): Option[List[out.Node]] =
		for((target, value) <- getTargetAndValue(g)) yield
			out.ProcessingInstruction(target, value) :: reverseXML
			
	def trace(writer: Writer, level: Int, reference: ReferenceFunction) = basicTrace(writer, level, description)
	def nextPossiblePatterns = List(this)
}
trait PIValuePattern[Generated] extends ProcessingInstructionPattern[Generated]
{
	def targetAllowed(target: String): Boolean
	def targetDescription: String
	def data: ValueParser[Generated]
	def getTarget(g: Generated): Option[String]
	
	def getTargetAndValue(g: Generated) =
	{
		for(target <- getTarget(g);
			value <- data.marshalToString(g))
		yield
			(target, value)
	}
	def generate(target: String, dataValue: String): Option[Generated] =
	{
		if(targetAllowed(target))
			data.generate(dataValue)
		else
			None
	}
	def description = "processing instruction {target='" + targetDescription + "', data='" + data.dataDescription + "'}"
}
class BasicPIValuePattern[Generated](val target: String, val data: ValueParser[Generated]) extends PIValuePattern[Generated]
{
	assume(target != null, "Processing instruction target cannot be null")
	assume(!target.isEmpty, "Processing instruction target cannot be empty")
	
	def getTarget(g: Generated) = Some(target)
	def targetAllowed(t: String) = target == t
	def targetDescription = target
}