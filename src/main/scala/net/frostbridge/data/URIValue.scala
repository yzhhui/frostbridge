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

import java.net.{MalformedURLException, URI, URISyntaxException, URL}

abstract class URIValue extends BasicParser[URI]
{
	def parse(value: String) = Some(new URI(value))
	def stringify(uri: URI) = Some(uri.toString)
}
object AnyURI extends URIValue
{
	def isAllowed(uri: URI) = true
	def dataDescription: String = "URI"
}

abstract class URLValue extends BasicParser[URL]
{
	def parse(value: String) = Some(new URL(value))
	def stringify(url: URL) = Some(url.toString)
}

object AnyURL extends URLValue
{
	def isAllowed(url: URL) = true
	def dataDescription: String = "URL"
}