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

// still requires tests for when index is out of bounds or other exceptional conditions
object TListSpecification extends Properties("TList")
{
	implicit val tListGen: Arbitrary[TList[Int]] = Arbitrary(for(list <- listOf(arbitrary[Int])) yield generateList(list, TList.empty))
	private def generateList(fromList: List[Int], prependTo: TList[Int]): TList[Int] =
	{
		if(fromList.isEmpty)
			prependTo
		else
			generateList(fromList.tail, fromList.head :: prependTo)
	}
	
	specify("apply correct", (list: TList[Int]) =>
		list.elements.zipWithIndex.forall(elementIndexPair => list(elementIndexPair._2) == elementIndexPair._1))
	
	specify("equal lists imply equal hashCodes",
		(list1: TList[Int], list2: TList[Int]) => (list1 == list2) ==> (list1.hashCode == list2.hashCode))
	specify("equals correct", (list1: TList[Int], list2: TList[Int]) => (list1 == list2) == (list1 sameElements list2) )
	specify("equals reflexive", (list: TList[Int]) => (list == list))
	specify("equals symmetric", (list1: TList[Int], list2: TList[Int]) => (list1 == list2) == (list2 == list1))
	specify("equals transitive",
		(list1: TList[Int], list2: TList[Int], list3: TList[Int]) => (list1 == list2 && list2 == list3) ==> (list1 == list3))

	specify("create single length", (i: Int) => TList(i).length == 1)
	specify("create single correct", (i: Int) => TList(i).apply(0) == i)
	
	specify("prepend correct value", (i: Int, list: TList[Int]) => (i :: list).apply(0) == i)
	specify("prepend increments length", (i: Int, list: TList[Int]) => (i :: list).length == list.length + 1)
	specify("prepend correct", (i: Int, list: TList[Int]) => (i :: list) sameElements (TList(i) ++ list))
	
	specify("reverse preserves length", (list: TList[Int]) => list.reverse.length == list.length)
	specify("double reverse equals original", (list: TList[Int]) => list.reverse.reverse == list)
	specify("reverse correct", (list: TList[Int]) => list.reverse sameElements list.toArray.projection.reverse)
	specify("reverse_::: length correct", (list1: TList[Int], list2: TList[Int]) => (list1 reverse_::: list2).length == list1.length + list2.length)
	specify("reverse_::: correct", (list1: TList[Int], list2: TList[Int]) => (list1 reverse_::: list2) sameElements (list1.reverse ++ list2))
	
	specify("make length correct", (size: Int, i: Int) => (size >= 0) ==> (TList.make(size, i).length == size))
	specify("make contains correct value", (size: Int, value: Int) => (size >= 0) ==> TList.make(size, value).forall(e => e == value))
	
	specify("range length correct", 
		(a: Int, b: Int) =>
		{
			val (start, end) = if(a <= b) (a, b) else (b, a)
			TList.range(start, end).length == end - start + 1
		})
	specify("range contents correct",
		(a: Int, b: Int) =>
		{
			val (start, end) = if(a <= b) (a, b) else (b, a)
			TList.range(start, end).elements.zipWithIndex.forall(e => (e._1 - start) == e._2)
		})
}