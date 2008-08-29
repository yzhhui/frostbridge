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

class EnumerationByName[T <: Enumeration](val enum: T, val dataDescription: String) extends BasicParser[T#Value]
{
	def parse(string: String) = enum.elements.find(string == _.toString)
	def stringify(value: T#Value) = Some(value.toString)
	def isAllowed(value: T#Value) = true
}
class EnumerationByID[T <: Enumeration](val enum: T, val dataDescription: String) extends BasicParser[T#Value]
{
	def parse(string: String) = Some(enum(string.toInt))
	def stringify(value: T#Value) = Some(value.id.toString)
	def isAllowed(value: T#Value) = true
}
