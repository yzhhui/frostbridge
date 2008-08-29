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

sealed trait CanonicalMap[A <: NotNull]
{
	def intern(value: A): A
	def size: Int
}

final class WeakCanonicalMap[A <: NotNull] extends CanonicalMap[A]
{
	import java.util.WeakHashMap
	import java.lang.ref.{Reference, WeakReference}
	private val backing = new WeakHashMap[A, Reference[A]]

	def intern(value: A): A =
	{
		def addValue: A =
		{
			backing.put(value, new WeakReference[A](value))
			value
		}
		val existingRef = backing.get(value)
		if(existingRef == null)
			addValue
		else
		{
			val referenceValue = existingRef.get
			if(referenceValue == null)
				addValue
			else
				referenceValue
		}
	}
	
	def size = backing.size
}