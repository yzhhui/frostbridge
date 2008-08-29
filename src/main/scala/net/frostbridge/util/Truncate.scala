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
package net.frostbridge.util

/** Utility methods for truncating a string to a maximum length.*/
object TruncateString
{
	/** The maximum number of characters of a string returned from truncate. */
	val MaximumLength = 30
	/** The algorithm must be be adjusted if the length of this is changed. */
	val Separator = " ... "
	
	assume(MaximumLength > 3*Separator.length, "Truncated length must be reasonably larger than the size of the separator.")
	
	/** Ensures that the string 's' is no longer than 30 characters.  If 's' is longer than
	* 30 characters, a smaller string is created by removing middle characters and inserting
	* " ... "
	*/
	def apply(s: String): String =
	{
		val length = s.length
		if(length <= MaximumLength)
			s
		else
		{
			val usableLength = MaximumLength - Separator.length
			val third = usableLength / 3
			s.substring(0, usableLength - third) + Separator + s.substring(length - third, length)
		}
	}
}