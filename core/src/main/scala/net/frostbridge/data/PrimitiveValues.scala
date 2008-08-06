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

abstract class CharacterValue extends BasicParser[Char]
{
	def parse(value: String) =
	{
		if(value.length == 1)
			Some(value.charAt(0))
		else
			None
	}
	def stringify(c: Char) = Some(c.toString)
	def typeDescription = "character"
}
object AnyCharacter extends CharacterValue
{
	def isAllowed(c: Char) = true
	def dataDescription: String = "character"
}
object AlphanumericCharacter extends CharacterValue
{
	def isAllowed(c: Char) = Character.isLetterOrDigit(c)
	def dataDescription: String = "alphanumeric character"
}

abstract class DoubleValue extends BasicParser[Double]
{
	def parse(value: String) = Some(value.toDouble)
	def stringify(value: Double) = Some(value.toString)
}
object AnyDouble extends DoubleValue
{
	def isAllowed(value: Double) = true
	def dataDescription = "double"
}
class RangedDouble(range: Ranged[Double]) extends DoubleValue
{
	def isAllowed(value: Double) = range.contains(value)
	def dataDescription = range.description("double")
}
class BooleanValue extends BasicParser[Boolean]
{
	def parse(value: String) = Some(value.toLowerCase.equals("true"))
	def stringify(b: Boolean) = Some(b.toString)
	def isAllowed(value: Boolean) = true
	def dataDescription = "boolean"
}
object BooleanValue extends BooleanValue

abstract class IntegerValue extends BasicParser[Int]
{
	def parse(value: String) = Some(value.toInt)
	def stringify(value: Int) = Some(value.toString)
	def typeDescription = "integer"
}

object AnyInteger extends IntegerValue
{
	def isAllowed(value: Int) = true
	def dataDescription = "integer"
}
class RangedInteger(range: Ranged[Int]) extends IntegerValue
{
	def isAllowed(value: Int) = range.contains(value)
	def dataDescription = range.description("integer")
}
object PositiveInteger extends IntegerValue
{
	def isAllowed(value: Int) = value > 0
	def dataDescription = "positive integer"
}
object NonPositiveInteger extends IntegerValue
{
	def isAllowed(value: Int) = value <= 0
	def dataDescription = "non-positive integer"
}
object NegativeInteger extends IntegerValue
{
	def isAllowed(value: Int) = value < 0
	def dataDescription = "negative integer"
}
object NonNegativeInteger extends IntegerValue
{
	def isAllowed(value: Int) = value >= 0
	def dataDescription = "non-negative integer"
}
