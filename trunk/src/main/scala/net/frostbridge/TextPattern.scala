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
import PatternFactory._
import data.ValueParser
import util.TList


/** A pattern that has some single content value (Text or a Comment) */
sealed trait ContentPattern[Generated] extends UnmatchedPattern[Generated] with BasicMarshaller[Generated]
{
	def content: ValueParser[Generated]
	
	protected def marshalImpl(g: Generated, reverseXML: TList[out.Node]) =
	{
		for(string <- content.marshalToString(g)) yield
			nodeFromString(string) :: reverseXML
	}
	
	protected def nodeFromString(string: String): out.Node
		
	def trace(writer: Writer, level: Int, reference: ReferenceFunction) = basicTrace(writer, level, description)
	
	def nextPossiblePatterns = List(this)
}

/**
* A pattern that matches text and generates a value using a ValueParser.
*/
case class TextPattern[Generated](content: ValueParser[Generated]) extends ContentPattern[Generated]
{
	lazy val matchEmpty = content.generate("")
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


/**
* A pattern that matches a comment and generates a value from the comment content using a ValueParser.
*/
case class CommentPattern[Generated](content: ValueParser[Generated]) extends ContentPattern[Generated]
{
	def matchEmpty = None
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

/** A pattern that matches a processing instruction and generates a value from its 
* target and data. */
abstract class ProcessingInstructionPattern[Generated] extends UnmatchedPattern[Generated] with BasicMarshaller[Generated]
{
	def generate(pi: in.ProcessingInstruction): Option[Generated]
	def getTargetAndValue(g: Generated): Option[out.ProcessingInstruction]
	
	def matchEmpty = None
	
	def derive(node: in.Node) =
	{
		node match
		{
			case pi: in.ProcessingInstruction => generate(pi).map(EmptyPattern(_)).getOrElse(NotAllowedPattern)
			case close: in.Close => this
			case _ => NotAllowedPattern
		}
	}
	def marshalImpl(g: Generated, reverseXML: TList[out.Node]): Option[TList[out.Node]] =
		for(pi <- getTargetAndValue(g)) yield
			pi :: reverseXML
			
	def trace(writer: Writer, level: Int, reference: ReferenceFunction) = basicTrace(writer, level, description)
	def nextPossiblePatterns = List(this)
}
abstract class PIValuePattern[Generated] extends ProcessingInstructionPattern[Generated]
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
			out.ProcessingInstruction(target, value)
	}
	def generate(pi: in.ProcessingInstruction): Option[Generated] =
	{
		if(targetAllowed(pi.target))
			data.generate(pi.data)
		else
			None
	}
	def description = "processing instruction {target='" + targetDescription + "', data='" + data.dataDescription + "'}"
}
case class BasicPIValuePattern[Generated](target: String, data: ValueParser[Generated])
	extends PIValuePattern[Generated]
{
	assume(target != null, "Processing instruction target cannot be null")
	assume(!target.isEmpty, "Processing instruction target cannot be empty")
	
	def getTarget(g: Generated) = Some(target)
	def targetAllowed(t: String) = target == t
	def targetDescription = target
}