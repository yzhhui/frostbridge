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
import util.TList


trait Translator[Generated,Source] extends NotNull
{
	def process(s: Source): Generated
	def unprocess(g: Generated): Option[Source]
}
/**
* A pattern that delegates to another pattern and processes that pattern's generated result.
*/
private trait TranslatingPattern[Generated, Source] extends UnmatchedPattern[Generated] with MarshalErrorTranslator[Generated]
{	
	val delegate: Pattern[Source]
	assume(delegate != this)
	
	def translator: Translator[Generated, Source]
	
	def description = delegate.description
	final def nextPossiblePatterns = delegate.nextPossiblePatterns
	final def trace(writer: Writer, level: Int, reference: ReferenceFunction) = delegate.trace(writer, level, reference)
	
	def marshal(g: Generated, reverseXML: TList[out.Node]) =
		translateMarshalError(g)
		{
			translator.unprocess(g) match
			{
				case Some(source) => delegate.marshal(source, reverseXML)
				case None => Left(RootMarshalException(g, this))
			}
		}
	final def derive(node: in.Node) = translate(delegate.derive(node), translator)
	lazy val matchEmpty = delegate.matchEmpty.map(translator.process)
}

private[frostbridge] trait TranslatePatternFactory
{
	final def translate[Generated, Source]
		(pattern: Pattern[Source], translator: Translator[Generated, Source]): Pattern[Generated] =
	{
		pattern.ifValid
		{
			pattern.matched match
			{
				case Some(value) => emptyPattern(translator.process(value))
				case None => BasicTranslatingPattern(pattern, translator)
			}
		}
	}
}

/**
* A simple implementation of TranslatingPattern.
*/
private[frostbridge] final case class BasicTranslatingPattern[Generated, Source]
	(delegate: Pattern[Source], translator: Translator[Generated,Source])
	extends TranslatingPattern[Generated,Source]
{
	lazy val hash = List(getClass, delegate, translator).hashCode
}
