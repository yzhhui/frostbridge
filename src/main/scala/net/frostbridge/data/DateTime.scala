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

import DateTime.factory
import javax.xml.datatype.{DatatypeFactory, Duration, XMLGregorianCalendar}

abstract class DateTimeValue extends BasicParser[XMLGregorianCalendar]
{
	def parse(value: String) = Some(factory.newXMLGregorianCalendar(value))
	def stringify(value: XMLGregorianCalendar) = Some(value.toXMLFormat)
}

object AnyDateTime extends DateTimeValue
{
	def isAllowed(dateTime: XMLGregorianCalendar) = true
	def dataDescription: String = "any date and time"
}

abstract class DurationValue extends BasicParser[Duration]
{
	def parse(value: String) = Some(factory.newDuration(value))
	def stringify(value: Duration) = Some(value.toString)
}
object AnyDuration extends DurationValue
{
	def isAllowed(duration: Duration) = true
	def dataDescription: String = "any duration"
}

object DateTime
{
	val factory = DatatypeFactory.newInstance
}