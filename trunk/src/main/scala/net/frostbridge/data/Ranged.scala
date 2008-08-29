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
package net.frostbridge.data

object Ranged
{
	val DoubleOrdering: Ordering[Double] =
		new Ordering[Double]
		{
			override def compare(x: Double, y: Double): Int =
				if(x > y)
					1
				else if(x < y)
					-1
				else
					0
			override def equiv(x: Double, y: Double): Boolean = x == y
			override def gt(x: Double, y: Double): Boolean = x > y
			override def gteq(x: Double, y: Double): Boolean = x >= y
			override def lt(x: Double, y: Double): Boolean = x < y
			override def lteq(x: Double, y: Double): Boolean = x <= y
		}
	val IntegerOrdering: Ordering[Int] =
		new Ordering[Int]
		{
			override def compare(x: Int, y: Int): Int =
				if(x > y)
					1
				else if(x < y)
					-1
				else
					0
			override def equiv(x: Int, y: Int): Boolean = x == y
			override def gt(x: Int, y: Int): Boolean = x > y
			override def gteq(x: Int, y: Int): Boolean = x >= y
			override def lt(x: Int, y: Int): Boolean = x < y
			override def lteq(x: Int, y: Int): Boolean = x <= y
		}
	
	implicit def double2Ranged(value: Double): Ranged[Double] =
		new Ranged(
			MinInclusive[Double, Ordering[Double]](value)(DoubleOrdering),
			MaxInclusive[Double, Ordering[Double]](value)(DoubleOrdering))(DoubleOrdering)
	implicit def int2Ranged(value: Int): Ranged[Int] =
		new Ranged(
			MinInclusive[Int, Ordering[Int]](value)(IntegerOrdering),
			MaxInclusive[Int, Ordering[Int]](value)(IntegerOrdering))(IntegerOrdering)
}

case class Ranged[T](val minimum: MinimumOrNone[T], val maximum: MaximumOrNone[T])
	(implicit val ordering: Ordering[T]) extends NotNull
{
	def this(copy: Ranged[T]) = this(copy.minimum, copy.maximum)(copy.ordering)
	def to(other: T) = Ranged(minimum, MaxInclusive[T, Ordering[T]](other)(ordering))
	def until(other: T) = Ranged(minimum, MaxExclusive[T, Ordering[T]](other)(ordering))
	
	def description(typeDescription: String) = typeDescription + minimum.toString + maximum.toString
	def contains(value: T): Boolean = minimum(value) && maximum(value)
}


sealed trait Constraint[T] extends NotNull
{
	def apply(v: T): Boolean
}
sealed trait MinimumOrNone[T] extends Constraint[T]
sealed trait MaximumOrNone[T] extends Constraint[T]
final case class MinInclusive[T, O <: Ordering[T]](min: T)(implicit val ordering: O) extends MinimumOrNone[T]
{
	def apply(v: T) = ordering.lteq(min, v)
	override def toString = " <= " + min
}
final case class MinExclusive[T, O <: Ordering[T]](min: T)(implicit val ordering: O) extends MinimumOrNone[T]
{
	def apply(v: T) = ordering.lt(min, v)
	override def toString = " < " + min
}
final case class MaxInclusive[T, O <: Ordering[T]](max: T)(implicit val ordering: O) extends MaximumOrNone[T]
{
	def apply(v: T) = ordering.gteq(max, v)
	override def toString = " >= " + max
}
final case class MaxExclusive[T, O <: Ordering[T]](max: T)(implicit val ordering: O) extends MaximumOrNone[T]
{
	def apply(v: T) = ordering.gt(max, v)
	override def toString = " > " + max
}
final case object NoConstraint extends MinimumOrNone[Nothing] with MaximumOrNone[Nothing]
{
	def apply(v: Nothing) = true
	override def toString = ""
}