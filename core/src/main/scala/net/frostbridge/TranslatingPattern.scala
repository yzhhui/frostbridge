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

import PatternFactory._
import PatternImpl.translateNotAllowed
import Traceable.{basicTrace, ReferenceFunction}

/**
* A pattern that delegates to another pattern and processes that pattern's generated result.
*/
private trait TranslatingPattern[Generated, Source] extends UnmatchedPattern[Generated] with MarshalErrorTranslator[Generated]
{	
	val delegate: Pattern[Source]
	assume(delegate != this)
	
	def unprocess: (Generated => Option[Source])
	def process: (Source => Generated)
	
	def description = delegate.description
	final def nextPossiblePatterns = delegate.nextPossiblePatterns
	final def trace(writer: Writer, level: Int, reference: ReferenceFunction) = delegate.trace(writer, level, reference)
	

	def marshal(g: Generated, reverseXML: List[out.Node]) =
		translateMarshalError(g)
		{
			unprocess(g) match
			{
				case Some(source) => delegate.marshal(source, reverseXML)
				case None => Left(RootMarshalException(g, this))
			}
		}
	def derive(node: in.Node) = translate(delegate.derive(node), process, unprocess)
	lazy val matchEmpty = delegate.matchEmpty.map(process)
}

private[frostbridge] trait TranslatePatternFactory
{
	def translate[Generated, Source](pattern: Pattern[Source],
		process: (Source => Generated), unprocess: (Generated => Option[Source])): Pattern[Generated] =
	{
		pattern.ifValid
		{
			pattern.matched match
			{
				case Some(value) => emptyPattern(process(value))
				case None => new BasicTranslatingPattern(pattern, process, unprocess)
			}
		}
	}
}

/**
* A simple implementation of TranslatingPattern.
*/
private[frostbridge] final class BasicTranslatingPattern[Generated, Source]
	(val delegate: Pattern[Source], val process: (Source => Generated), val unprocess: (Generated => Option[Source]))
	extends TranslatingPattern[Generated,Source]
