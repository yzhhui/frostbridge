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

import org.scalacheck._
import Prop._

class FrostbridgeSpecification extends Properties("Frostbridge")
{
	include(PatternSpecification)
	include(util.TruncateSpecification)
	include(util.TListSpecification)
	include(util.CanonicalMapSpecification)
	import xml.ArbitraryXML
	specify("XML Test", forAll(ArbitraryXML.defaultXML)((x: String) => true ))
}
object FrostbridgeSpecification extends FrostbridgeSpecification
