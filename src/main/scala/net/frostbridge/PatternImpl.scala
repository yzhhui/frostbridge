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
import util.TList
import Traceable.{basicTrace, ReferenceFunction}

// classes to implement Pattern.derive


trait MarshalInvalid[Generated] extends UnmatchedPattern[Generated]
{
	final def marshal(g: Generated, reverseXML: TList[out.Node]) = Left(RootMarshalException(g, this))
}
trait BasicMarshaller[Generated] extends UnmatchedPattern[Generated]
{
	final def marshal(g: Generated, reverseXML: TList[out.Node]) = marshalImpl(g, reverseXML).toRight(RootMarshalException(g, this))
	protected def marshalImpl(g: Generated, reverseXML: TList[out.Node]): Option[TList[out.Node]]
}
trait MarshalErrorTranslator[Generated] extends UnmatchedPattern[Generated]
{
	def translateMarshalError(value: Generated)(e: Either[MarshalException[_], TList[out.Node]]):
		Either[MarshalException[Generated], TList[out.Node]] =
			e.left.map(error => ChainedMarshalException(value, this)(error :: Nil))
}


/**
* The instance of NotAllowedPattern that is used directly and then
* implicitly converted (casted) to the right Generated type (the Generated type
* of a NotAllowedPattern is unimportant since it is never matched).
*/
case object NotAllowedPattern extends NotAllowedPattern[Nothing]
{
	val hash = System.identityHashCode(this)
}

/**
* A pattern that represents a match error.
*/
sealed trait NotAllowedPattern[Generated] extends UnmatchedPattern[Generated] with MarshalInvalid[Generated]
{
	// no need to memoize
	final def derive(node: in.Node) = this
	
	final def matchEmpty = None
	
	override final def valid = false
	
	final def nextPossiblePatterns = error("No possible patterns for the Not Allowed pattern")
	final def description = error("Description is not valid for the Not Allowed pattern")
	
	final def trace(writer: Writer, level: Int, reference: ReferenceFunction) = basicTrace(writer, level, "not allowed")
}

private object PatternImpl
{
	/** This implicit is used to convert the NotAllowedPattern object to Pattern[T] for arbitrary T.
	* This is legitimate because NotAllowedPattern's behavior is independent of T.
	*/
	implicit def translateNotAllowed[T](p: NotAllowedPattern[Nothing]): NotAllowedPattern[T] =
		NotAllowedPattern.asInstanceOf[NotAllowedPattern[T]]
	
	/** Asserts that the given pattern is not the empty pattern. */
	def checkNonEmpty(pattern: Pattern[_]) =
	{
		if(!pattern.matched.isEmpty)
			error("Pattern cannot be empty")
	}
	/** Asserts that the given patterns are not the empty pattern. */
	def checkNonEmpty(pattern1: Pattern[_], pattern2: Pattern[_]) =
	{
		if(!pattern1.matched.isEmpty)
			error("Patterns cannot be empty (first was empty)")
		else if(!pattern2.matched.isEmpty)
			error("Patterns cannot be empty (second was empty)")
	}
	/** Asserts that the given pattern is valid. */
	def checkAllowed(pattern1: Pattern[_], pattern2: Pattern[_]) =
	{
		assume(pattern1.valid, "Patterns cannot be 'not allowed' (first was 'not allowed')")
		assume(pattern2.valid, "Patterns cannot be 'not allowed' (second was 'not allowed')")
	}
	/** Asserts that the given patterns are valid. */
	def checkAllowed(pattern: Pattern[_]) = assume(pattern.valid, "Pattern cannot be 'not allowed'")
	
}