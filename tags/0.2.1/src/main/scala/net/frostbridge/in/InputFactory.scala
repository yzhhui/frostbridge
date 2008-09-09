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
package net.frostbridge.in

import java.io.File
import java.net.URL

private[in] trait InputFactory extends NotNull
{
	import org.codehaus.stax2.XMLStreamReader2
	
	private[this] val factory =
	{
		import com.ctc.wstx.stax.WstxInputFactory
		import org.codehaus.stax2.XMLInputFactory2
		import XMLInputFactory2._
		import javax.xml.stream.XMLInputFactory
		import XMLInputFactory._
		
		val temp: XMLInputFactory2 = new WstxInputFactory()
		
		def setProperty(key: String, value: Any)
		{
			assume(temp.isPropertySupported(key), "Property '" + key + "' is not supported")
			temp.setProperty(key, value)
		}
		
		setProperty(P_INTERN_NAMES, internNames)
		setProperty(P_INTERN_NS_URIS, internNamespaceURIs)
		setProperty(P_REPORT_PROLOG_WHITESPACE, false)
		setProperty(P_PRESERVE_LOCATION, preserveLocation)
		setProperty(P_REPORT_CDATA, reportCDATA)
		setProperty(SUPPORT_DTD, supportDTD)
		setProperty(IS_SUPPORTING_EXTERNAL_ENTITIES, true)
		setProperty(IS_REPLACING_ENTITY_REFERENCES, true)
		setProperty(IS_VALIDATING, validate)
		setProperty(IS_COALESCING, coalesce)
		temp
	}

	def reportCDATA: Boolean
	def validate: Boolean
	def coalesce: Boolean
	def supportDTD: Boolean
	def preserveLocation: Boolean
	def internNames: Boolean
	def internNamespaceURIs: Boolean

	def createReader(file: File) =
	{
		factory.createXMLStreamReader(file)
	}
	def createReader(url: URL) =
	{
		factory.createXMLStreamReader(url)
	}
	import java.io.InputStream
	def createReader(stream: InputStream) =
	{
		factory.createXMLStreamReader(stream).asInstanceOf[XMLStreamReader2]
	}
	import java.io.Reader
	def createReader(reader: Reader) =
	{
		factory.createXMLStreamReader(reader).asInstanceOf[XMLStreamReader2]
	}
}

private[in] object DefaultInputFactory extends InputFactory
{
	def reportCDATA = false
	def validate = false
	def coalesce = true
	def supportDTD = false
	def preserveLocation = true
	def internNames = false
	def internNamespaceURIs = false
}