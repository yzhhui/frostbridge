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

sealed trait QName extends NotNull
{
	def namespaceURI: String
	def localPart: String
	def description: String
	def ::(namespaceURI: String): QName
}
/**
* A QName literal may be created by importing QName.localStringToQName
* and doing "<URI>"::"localPart"
*/
private[frostbridge] final class QNameImpl(val namespaceURI: String, val localPart: String) extends QName
{
	assume(!localPart.isEmpty, "Local part cannot be empty")
	
	def ::(namespaceURI: String) =
	{
		if(this.namespaceURI.isEmpty)
			new QNameImpl(util.Check(namespaceURI), localPart)
		else
			throw new IllegalArgumentException("Local name '" + localPart + "' already in namespace URI '" + this.namespaceURI + "' (attempted to apply '" + namespaceURI + "')")
	}

	def description =
		if(namespaceURI == "")
			localPart
		else
			"{" + namespaceURI + "}" + localPart
			
	override def hashCode = namespaceURI.hashCode * 41 + localPart.hashCode
	override def equals(o: Any) =
	{
		o match
		{
			case q: QNameImpl => (this eq q) || ((q.namespaceURI == namespaceURI) && (q.localPart == localPart))
			case _ => false
		}
	}
			
	override def toString = description
}

object QName
{
	implicit def localStringToQName(localPart: String): QName = new QNameImpl("", util.Check(localPart))
	def apply(namespaceURI: String, localPart: String): QName = new QNameImpl(util.Check(namespaceURI), util.Check(localPart))
}