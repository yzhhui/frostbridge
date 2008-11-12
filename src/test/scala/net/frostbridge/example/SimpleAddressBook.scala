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
package net.frostbridge.example

import net.frostbridge._
import Implicits._

import java.io.{File, OutputStreamWriter}

object SimpleAddressBook
{
	// define the pattern
	val addressBookPattern: Pattern[AddressBook] =
	{
		import data._
		import PatternFactory._
		
		val name = attribute("name", NonEmptyString)
		val phone = attribute("phone", AnyString)
		val contactPattern = name :+: phone :+: (IgnoreAny.anyAttribute*)
		val contact =
			new element[Contact, contactPattern.GeneratedType]("contact", contactPattern)
			{
				def generate(c: contactPattern.GeneratedType) =
				{
					val n :+: p :+: ignore = c
					Contact(n, p)
				}
				def marshalTranslate(c: Contact) = Some(c.name :+: c.phone :+: Nil)
			}
		
		new element[AddressBook, Seq[Contact]]("addrBook", contact*)
		{
			def generate(contacts: Seq[Contact]) = AddressBook(contacts)
			def marshalTranslate(book: AddressBook) = Some(book.contacts)
		}
	}
	
	// start serialization/deserialization and handle result
	def test(filename: String)
	{
		val source = in.StAXStream(new File(filename))
		Unmarshaller.unmarshalOrError(addressBookPattern, source) match
		{
			case Right(parsed) =>
			{
				println("\nSuccessfully unmarshalled address book document with " + parsed.contacts.length + " contacts.\n")
				println(parsed.toString)
				
				val writer = new OutputStreamWriter(System.out)
				Marshaller(parsed, addressBookPattern, writer) match
				{
					case None => println("\n\nMarshalled successfully.")
					case Some(error) => println(error)
				}
			}
			case Left(e) => println(e.toString)
		}
	}
}

// data types
case class AddressBook(contacts: Seq[Contact])
case class Contact(name: String, phone: String)