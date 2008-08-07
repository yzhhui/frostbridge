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

import NameClass.exceptToString

sealed trait NameClass extends NotNull
{
	def matches(qname: QName): Boolean
	def description: String
	
	override def toString = "NameClass: " + description
	
	def | (other: NameClass): NameClass = NameChoice(this, other)
	def unary_! : NameClass = AnyNameExcept(this)
}

case object AnyName extends NameClass
{
	def matches(qname: QName) = true
	def description = "*"
}
final case class AnyNameExcept(except: NameClass) extends NameClass
{
	def matches(qname: QName) = !except.matches(qname)
	def description = exceptToString("*", except)
	override def unary_! : NameClass = except
}

final case class NsName(namespaceURI: String) extends NameClass
{
	assume(namespaceURI != null, "NamespaceURI cannot be null")
	def matches(qname: QName) = namespaceURI == qname.namespaceURI
	def description = "{" + namespaceURI.toString + "}*"
}
final case class NsNameExcept(namespaceURI: String, except: NameClass) extends NameClass
{
	assume(namespaceURI != null, "NamespaceURI cannot be null")
	def matches(qname: QName) = namespaceURI == qname.namespaceURI && !except.matches(qname)
	def description = exceptToString("{" + namespaceURI.toString + "}*", except)
}

final case class Name(name: QName) extends NameClass
{
	def matches(qname: QName) = name.equals(qname)
	def description = name.description
}
final case class NameChoice(first: NameClass, second: NameClass) extends NameClass
{
	def matches(qname: QName) = first.matches(qname) || second.matches(qname)	
	def description = first.description + " | " + second.description
}

object NameClass
{
	implicit def qNameToClass(qname: QName): Name = Name(qname)
	implicit def localPartToClass(localPart: String): Name = Name(util.Check(localPart))
	
	 def exceptToString(basic: String, except: NameClass): String =
		basic + "-[ " + except.description + " ]"
}