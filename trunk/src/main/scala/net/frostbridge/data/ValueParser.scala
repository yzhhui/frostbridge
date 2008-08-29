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

trait ValueParser[T] extends NotNull
{
	def generate(value: String): Option[T]
	def marshalToString(value: T): Option[String]
	
	def dataDescription: String
}

abstract class BasicParser[T] extends ValueParser[T]
{
	def generate(string: String): Option[T] =
	{
		try
		{
			parse(string).filter(isAllowed)
		}
		catch
		{
			case e: Exception => None
		}
	}
	def marshalToString(value: T): Option[String] =
	{
		if(isAllowed(value))
		{
			try
			{
				stringify(value)
			}
			catch
			{
				case e: Exception => None
			}
		}
		else
			None
	}

	def parse(string: String): Option[T]
	def stringify(value: T): Option[String]
	def isAllowed(value: T): Boolean
}