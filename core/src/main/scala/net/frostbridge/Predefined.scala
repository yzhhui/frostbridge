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

/** The Predefined object collects as many of the useful implicits for client code to
* import from one place.  Additionally, abbreviated pattern names and some simplified
* pattern creators are provided.
*/
object Predefined
{
	import data._
	
	//Collection of all implicits commonly used in client code
	implicit def qNameToClass(qname: QName): Name = NameClass.qNameToClass(qname)
	implicit def localPartToClass(localPart: String): Name = NameClass.localPartToClass(localPart)
	implicit def localStringToQName(localPart: String): QName = QName.localStringToQName(localPart)
	implicit def double2Ranged(value: Double): Ranged[Double] = Ranged.double2Ranged(value)
	implicit def int2Ranged(value: Int): Ranged[Int] = Ranged.int2Ranged(value)
	implicit def any2Anchor[A](a: A): Anchor[A] = new Anchor[A](a)
	
	// Simplified constructors for some patterns
	def Any_@(nameClass: NameClass) = new SimpleAttributePattern[String](nameClass, AnyString)
	def A1_@(nameClass: NameClass) = new SimpleAttributePattern[String](nameClass, AlphanumericString)
	def Double_@(nameClass: NameClass) = new SimpleAttributePattern[Double](nameClass, AnyDouble)
	
	// Abbreviated aliases for some patterns
	type Attribute[Generated] = SimpleAttributePattern[Generated]
	type Element[Generated, ChildGenerated] = BasicElementPattern[Generated, ChildGenerated]
	type TextElement[Generated] = TextElementPattern[Generated]
	type Text[Generated] = BasicTextPattern[Generated]
}

/** Left associative pair construction/deconstruction */
object +++
{
	def unapply[A, B](pair: (A, B)): Some[(A, B)] = Some((pair._1, pair._2))
}
/** Right associative pair construction/deconstruction */
object :+:
{
	def unapply[A, B](pair: (A, B)): Some[(A, B)] = Some((pair._1, pair._2))
}
final case class ExtendedTuple[A,B](a: A, b: B) extends Tuple2(a, b)
{
	def :+:[C](c: C): ExtendedTuple[C,(A,B)] = ExtendedTuple(c, this)
	def +++[C](c: C): ExtendedTuple[(A,B),C] = ExtendedTuple(this, c)
}
final case class Anchor[A](a: A)
{
	def :+:[B](b: B): ExtendedTuple[B,A] = ExtendedTuple(b, a)
	def +++[B](b: B): ExtendedTuple[A,B] = ExtendedTuple(a, b)
}