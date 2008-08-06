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

/**
* A QName literal may be created by importing QName.localStringToQName
* and doing "<URI>"::"localPart"
*/
final class QName(val namespaceURI: String with NotNull, val localPart: String with NotNull) extends NotNull
{
	assume(!localPart.isEmpty, "Local part cannot be empty")
	
	def ::(namespaceURI: String) =
	{
		if(this.namespaceURI.isEmpty)
			new QName(util.Check(namespaceURI), localPart)
		else
			throw new IllegalArgumentException("Local name '" + localPart + "' already in namespace URI '" + this.namespaceURI + "' (attempted to apply '" + namespaceURI + "')")
	}
	
	override def equals(other: Any) =
	{
		other match
		{
			case qname: QName => equalsQName(qname)
			case _ => false
		}
	}
	def equalsQName(qname: QName) =
		namespaceURI == qname.namespaceURI && localPart == qname.localPart

	def description =
		if(namespaceURI == "")
			localPart
		else
			"{" + namespaceURI + "}" + localPart
			
	override def toString = description
}

object QName
{
	implicit def localStringToQName(localPart: String): QName = new QName("", util.Check(localPart))
}