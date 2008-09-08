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

import PatternImpl.{checkAllowed, checkNonEmpty, translateNotAllowed}
import Traceable.{basicTrace, ReferenceFunction}
import util.TList

/** Represents a general transformation of the result of a pattern into another pattern.
* (p: Pattern[S]) >>= (b: Binding[G, S])
* processes the generated object of p and produces a new pattern to be matched
* for the unmarshalling case.  For the marshalling case, it will do a reverse
* processing.
*
* This general case is like a flatMap/bind.  The class to implement
* is Binding, which strips the generated pattern of top level AttributePatterns.*/
sealed trait Transformation[Generated,Source] extends NotNull
{
	def bind(in: Source): Pattern[Generated]
	def unprocess(g: Generated): Option[Source]
}
/** This case of Transformation is like a flatMap/bind with the restriction
* that the generated pattern cannot contain AttributePatterns that are not
* a descendent of an ElementPattern.
*
* This constraint is necessary for consistent behavior because attributes are unordered.*/
trait Binding[Generated,Source] extends Transformation[Generated,Source]
{
	/* Generates a new pattern to match from the given matched value. 
	* Any AttributePatterns that are not a descendent of an ElementPattern will be stripped
	* when called by bind.
	* 
	* This is necessary for consistent unmarshalling behavior because attributes are unordered.*/
	def process(in: Source): Pattern[Generated]
	
	/** Generates a new pattern to match from the given matched value by calling process
	* and then deriving the generated pattern against a virtual Close node to eliminate
	* all AttributePatterns that are not a descendant of an ElementPattern.
	*
	* This constraint is necessary for consistent behavior because attributes are unordered.*/
	final def bind(in: Source): Pattern[Generated] = process(in).derive(virtualClose)
	
	private def virtualClose: in.Close = 
		new in.VirtualClose("virtual node",
			"Virtual node to remove top-level attributes from pattern generated in Binding.process.")
}
/** This special case of Transformation is like a map operation.  It maps the
* generated object of a Pattern during unmarshalling into another object.*/
abstract class Mapping[Generated,Source] extends Transformation[Generated,Source]
{
	def process(s: Source): Generated
	final def bind(s: Source): Pattern[Generated] = EmptyPattern(process(s))
}
/** This special case of Transformation is like a filter operation.  It translates
* the generated object of a Pattern during unmarshalling into another object.*/
abstract class Filter[G] extends Transformation[G, G]
{
	def allowed(in: G): Boolean
	
	final def process(in: G) =
		if(allowed(in))
			EmptyPattern(in)
		else
			NotAllowedPattern
		
	def unprocess(g: G): Option[G] =
		if(allowed(g))
			Some(g)
		else
			None
}
/**
* A pattern that delegates to another pattern and processes that pattern's generated result.
* The definition of that processing is in Binding, which can behave 
*/
private abstract class TranslatingPattern[Generated, Source]
	extends UnmatchedPattern[Generated] with MarshalErrorTranslator[Generated]
{
	val delegate: Pattern[Source]
	assume(delegate != this)
	checkNonEmpty(delegate)
	checkAllowed(delegate)
	
	def binding: Transformation[Generated, Source]
	
	def description = delegate.description
	final def nextPossiblePatterns = delegate.nextPossiblePatterns
	final def trace(writer: Writer, level: Int, reference: ReferenceFunction) = delegate.trace(writer, level, reference)
	
	def marshal(g: Generated, reverseXML: TList[out.Node]) =
		translateMarshalError(g)
		{
			binding.unprocess(g) match
			{
				case Some(source) => delegate.marshal(source, reverseXML)
				case None => Left(RootMarshalException(g, this))
			}
		}
	final def derive(node: in.Node) = delegate.derive(node) >>= binding
	lazy val matchEmpty = delegate.matchEmpty.flatMap(m => binding.bind(m).matchEmpty)
}

/**
* A simple implementation of TranslatingPattern.
*/
private[frostbridge] final class BasicTranslatingPattern[Generated, Source]
	(val delegate: Pattern[Source], val binding: Transformation[Generated,Source])
	extends TranslatingPattern[Generated,Source]
