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

import TruncateString._
import org.scalacheck._
import Prop._
import Gen.{choose, sized, vectorOf}
import Arbitrary.arbitrary

object TruncateSpecification extends Properties("TruncateString")
{
	def largeList[T](minimumSize: Int, g: => Gen[T]) =
		sized(additionalSize =>
			for(n <- choose(minimumSize, minimumSize+additionalSize); l <- vectorOf(n,g)) yield
				l.toList
		)
			
	def largeStringGen(minimumSize: Int) =
	{
		assume(minimumSize >= 0)
		largeList(minimumSize, arbitrary[Char]) suchThat (s => !s.contains(Separator)) map (_.toList) map (List.toString(_))
	}
	
	private def truncateAndSplit(s: String): Option[(String, String)] =
	{
		val truncated = TruncateString(s)
		val i = truncated.indexOf(Separator)
		if(i < 0)
			None
		else
			Some((truncated.substring(0, i), truncated.substring(i + Separator.length)))
	}
	
	specify("maximum length", (s: String) => TruncateString(s).length <= MaximumLength)
	specify("correct length",
		(s: String) => TruncateString(s).length == ( if(s.length >= MaximumLength) MaximumLength else s.length ) )
	specify("identity", (s: String) => if(s.length <= MaximumLength) TruncateString(s) == s else TruncateString(s) != s)
	
	specify("truncated contains separator", forAll(largeStringGen(MaximumLength+1))
		((s: String) => truncateAndSplit(s).isDefined))
	specify("starts with", forAll(largeStringGen(MaximumLength+1))
		((s: String) => s.startsWith(truncateAndSplit(s).get._1) ))
	specify("ends with", forAll(largeStringGen(MaximumLength+1))
		((s: String) => s.endsWith( truncateAndSplit(s).get._2 ) ))
	
}