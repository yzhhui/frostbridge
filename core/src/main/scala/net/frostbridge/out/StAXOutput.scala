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
package net.frostbridge.out

import javax.xml.stream.XMLStreamWriter

/**
* Notes: writing "" as the prefix will make the root element's namespace the default namespace
*/
object StAXOutput
{
	import javax.xml.stream.XMLOutputFactory._
	import org.codehaus.stax2.XMLOutputFactory2._
	import org.codehaus.stax2.XMLStreamProperties._
	import com.ctc.wstx.stax.WstxOutputFactory
	
	private val outputFactory =
	{
		val factory = new WstxOutputFactory
		factory.setProperty(IS_REPAIRING_NAMESPACES, true)
		factory.setProperty(P_AUTOMATIC_EMPTY_ELEMENTS, true)
		factory.setProperty(P_AUTOMATIC_NS_PREFIX, "p")
		factory.setProperty(XSP_NAMESPACE_AWARE, true)
		factory
	}
	
	import java.io.{OutputStream,Writer}
	def createWriter(writer: Writer) = outputFactory.createXMLStreamWriter(writer)
	def createWriter(stream: OutputStream) = outputFactory.createXMLStreamWriter(stream)
		
	import javax.xml.stream.XMLStreamException
	def write(nodes: List[Node], writer: XMLStreamWriter): Option[XMLStreamException] =
	{
		try
		{
			writer.writeStartDocument()
			nodes.foreach(_.write(writer))
			writer.writeEndDocument()
			None
		}
		catch
		{
			case e: XMLStreamException => Some(e)
		}
		finally
		{
			writer.close()
		}
	}
}

sealed trait Node
{
	def write(writer: XMLStreamWriter): Unit
}
final case class Attribute(name: QName, value: String) extends Node
{
	assume(value != null, "Attribute value cannot be null")
	def write(writer: XMLStreamWriter) = writer.writeAttribute("", name.namespaceURI, name.localPart, value)
	
	override def toString = name.localPart + "=\"" + value + "\""
}

final case class Element(name: QName, content: List[Node]) extends Node
{
	val (attributes, children) = content.partition(_.isInstanceOf[Attribute])
	
	def write(writer: XMLStreamWriter) =
	{
		writer.writeStartElement("", name.localPart, name.namespaceURI)
		attributes.foreach(_.write(writer))
		children.foreach(_.write(writer))
		writer.writeEndElement()
	}
	
	override def toString = "<" + name.localPart + " " + attributes.mkString(" ") + ">" +
		children.mkString + "</" + name.localPart + ">"
}
final case class Text(text: String) extends Node
{
	assume(text != null, "Text content cannot be null")
	def write(writer: XMLStreamWriter) = writer.writeCharacters(text)
	override def toString = text
}
final case class ProcessingInstruction(target: String, data: String) extends Node
{
	assume(target != null, "Processing instruction target cannot be null.")
	assume(data != null, "Processing instruction data cannot be null.")
	def write(writer: XMLStreamWriter) = writer.writeProcessingInstruction(target, data)
	override def toString = "<?" + target + " " + data + " ?>"
}
final case class Comment(text: String) extends Node
{
	assume(text != null, "Comment content cannot be null.")
	def write(writer: XMLStreamWriter) = writer.writeComment(text)
	override def toString = "<!--" + text + "-->"
}