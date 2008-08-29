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

import org.scalacheck._
import Prop._
import Gen.listOf
import Arbitrary.arbitrary

object CanonicalMapSpecification extends Properties("CanonicalMap")
{
	specify("identity when empty", (i: Int) =>
	{
		val map = new WeakCanonicalMap[TList[Int]]
		val i2 = intObject(i)
		map.intern(i2) eq i2
	})
	specify("intern existing", (i: Int) =>
	{
		val map = new WeakCanonicalMap[TList[Int]]
		val i2 = intObject(i)
		(map.intern(i2) eq i2) &&
		{
			val i3 = intObject(i)
			map.intern(i3) eq i2
		}
	})
	specify("weakly referenced", (i: Int) =>
	{
		val map = new WeakCanonicalMap[TList[Int]]
		map.intern(intObject(i))
		System.gc()
		val iDifferentObject = intObject(i)
		map.intern(iDifferentObject) eq iDifferentObject
	})
	
	private def intObject(i: Int): TList[Int] = TList[Int](i)
}