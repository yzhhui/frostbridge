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
package net.frostbridge.xml

import java.net.URI
import org.scalacheck.Gen

class NamespaceGenerator(schemeLength: Gen[Int], sspLength: Gen[Int], namespaceCountOption: Option[Int])
{
	private val namespaceG: Gen[String] =
	{
		for(scheme <- GenXMLStrings.alphaLowerString(schemeLength); ssp <- GenXMLStrings.nonEmptyString(sspLength)) yield
			new URI(scheme, ssp, null).toString
	}
	private val namespaces: Option[Array[String]] =
	{
		for(namespaceCount <- namespaceCountOption) yield
			Array.fromFunction(createNamespace _)(namespaceCount).flatMap(_.toList).toArray
	}
	
	private[this] def createNamespace(i: Int): Option[String] = namespaceG.sample
	
	def namespacesEnabled : Boolean = namespaceCountOption.isDefined
	
	def namespace: Option[Gen[String]] =
	{
		for(ns <- namespaces) yield
			for(index <- Gen.choose(0, ns.length - 1)) yield
				ns(index)
	}
}