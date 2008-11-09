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

object Random
{
	import org.scalacheck.{util => sutil, _}
	def poisson(expected: Double): Int =
	{
		val L = Math.exp(-expected)
		def poissonImpl(k: Int, p: Double): Int =
		{
			if(p < L)
				k
			else
				poissonImpl(k+1, p * sutil.StdRand.nextDouble)
		}
		poissonImpl(0, sutil.StdRand.nextDouble)
	}
	def poissonGen(expected: Double): Gen[Int] = Gen[Int] { (params: Gen.Params) => Some(poisson(expected)) }
	def pickN[T](n: Int, frequencyValues: Seq[(Int, T)]): List[T] =
	{
		require(n >= 0)
		val cumulative = accumulate(frequencyValues)
		def doPick(remaining: Int, workingList: List[T]): List[T] =
			if(remaining <= 0)
				workingList
			else
				doPick(remaining -1, pick(cumulative) :: workingList)
			
		doPick(n, Nil)
	}
	def pick1[T](frequencyValues: Seq[(Int, T)]): T = pick(accumulate(frequencyValues))
	private def pick[T](cumulative: (Int, List[(Int, T)])): T =
	{
		val (total, list) = cumulative
		require(total >= 1)
		val i = sutil.StdRand.nextInt(total)+1
		list.find(i <= _._1).get._2
	}
	private def accumulate[T](frequencyValues: Seq[(Int, T)]): (Int, List[(Int,T)]) =
	{
		val size = frequencyValues.size
		require(size != 0)
		def fold(result: (Int, List[(Int, T)]), current: (Int, T)): (Int, List[(Int, T)]) =
		{
			val newSum = result._1 + current._1
			(newSum, (newSum, current._2) :: result._2)
		}
		val reverse = ( (0, Nil: List[(Int, T)]) /: frequencyValues )(fold)
		(reverse._1, reverse._2.reverse)
	}
}