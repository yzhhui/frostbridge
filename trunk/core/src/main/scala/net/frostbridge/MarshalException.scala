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


/** Note that this does not actually extend Exception, so a name
* ending with something other than Exception would be nice.
*/
sealed trait MarshalException[T]
{
	def invalidObject: T
	def unmatchedPattern: Pattern[T]
	def getRootCauses: List[RootMarshalException[_]]
}

final case class ChainedMarshalException[T] (invalidObject: T, unmatchedPattern: Pattern[T])(causes: List[MarshalException[_]]) extends MarshalException[T]
{
	def getRootCauses = causes.flatMap(_.getRootCauses)
}

case class RootMarshalException[T](invalidObject: T, unmatchedPattern: Pattern[T]) extends MarshalException[T]
{
	def getRootCauses = List(this)
}
object MarshalException
{
	def unapply[T](me: MarshalException[T]): (T, Pattern[T]) =
	{
		me match
		{
			case ChainedMarshalException(invalidObject, unmatchedPattern) => (invalidObject, unmatchedPattern)
			case RootMarshalException(invalidObject, unmatchedPattern) => (invalidObject, unmatchedPattern)
		}
	}
}