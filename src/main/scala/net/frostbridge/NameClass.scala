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

import NameClass.{checkNamespaceURI, exceptToString}

/** Represents the set of qualified names acceptable for an attribute or element.
* The set of acceptable names should be non-empty.  Subclasses should generally
* make a reasonable effort to enforce this. */
sealed trait NameClass extends NotNull
{
	def matches(qname: QName): Boolean
	def description: String
	
	override def toString = "NameClass: " + description
	
	def | (other: NameClass): NameClass = NameChoice(this, other)
	def unary_! : NameClass = AnyNameExcept(this)
	private[frostbridge] def findNameClassOutside(nsURI: String): Option[NameClass] = Some(this)
}
/** All names are allowed. */
case object AnyName extends NameClass
{
	def matches(qname: QName) = true
	def description = "*"
	def - (other: NameClass) = AnyNameExcept(other)
}
/** All names are allowed except for the ones that match 'except'.
* 'except' may not be AnyName. */
final case class AnyNameExcept(except: NameClass) extends NameClass
{
	require(except != AnyName, "NameClass cannot be empty.")
	
	def matches(qname: QName) = !except.matches(qname)
	def description = exceptToString("*", except)
	override def unary_! : NameClass = except
}

/** All names with namespace URI 'namespaceURI' are allowed. */
final case class NsName(namespaceURI: String) extends NameClass
{
	require(namespaceURI != null, "NamespaceURI cannot be null")
	checkNamespaceURI(namespaceURI)
	
	def matches(qname: QName) = namespaceURI == qname.namespaceURI
	def description = "{" + namespaceURI.toString + "}*"
	def - (other: NameClass) = NsNameExcept(namespaceURI, other)
}
/** All names with namespace URI 'namespaceURI' except those that match
* 'except' are allowed. 'except' may only be of type Name or a NameChoice
* with Name leaves.  The Name instances must have namespace URI
* 'namespaceURI' */
final case class NsNameExcept(namespaceURI: String, except: NameClass) extends NameClass
{
	require(namespaceURI != null, "NamespaceURI cannot be null")
	require(except != AnyName, "NameClass cannot be the empty set (tried to exclude all names from namespace '" + namespaceURI + "')")
	for(invalidClass <- except.findNameClassOutside(namespaceURI))
		error("Only Name or NameChoice of Names with namespaceURI '" + namespaceURI + "' can be a descendant of NsNameExcept (found " + invalidClass + ")")
	
	def matches(qname: QName) = namespaceURI == qname.namespaceURI && !except.matches(qname)
	def description = exceptToString("{" + namespaceURI.toString + "}*", except)
}

/** The exact qualified name given by 'name' is allowed. */
final case class Name(name: QName) extends NameClass
{
	checkNamespaceURI(name.namespaceURI)
	def matches(qname: QName) = name.equals(qname)
	def description = name.description
	private[frostbridge] override def findNameClassOutside(nsURI: String) =
		if(name.namespaceURI == nsURI) None else Some(this)
}
/** The union of names in 'first' and 'second'.  AnyName is not allowed as a choice. */
final case class NameChoice(first: NameClass, second: NameClass) extends NameClass
{
	require(first != AnyName && second != AnyName)
	
	def matches(qname: QName) = first.matches(qname) || second.matches(qname)	
	def description = first.description + " | " + second.description
	private[frostbridge] override def findNameClassOutside(nsURI: String) =
		first.findNameClassOutside(nsURI) orElse second.findNameClassOutside(nsURI)
}

object NameClass
{
	/** Implicit conversion from a QName to a Name.*/
	implicit def qNameToClass(qname: QName): Name = Name(qname)
	/** Implicit conversion from a String representing the local part of a qualified name to
	* a Name.*/
	implicit def localPartToClass(localPart: String): Name = Name(util.Check(localPart))
	
	/** Verifies that the given namespace URI is not the namespace URI assigned to namespace
	* attributes.  Namespaces are handled by the framework and Woodstox and should not be
	* provided directly by the client. */
	private[frostbridge] def checkNamespaceURI(nsURI: String): Unit =
		require(nsURI != "http://www.w3.org/2000/xmlns", "Namespace URI cannot be xmlns namespace URI.")
	private[frostbridge] def exceptToString(basic: String, except: NameClass): String =
		basic + "-[ " + except.description + " ]"
}