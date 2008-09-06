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

import java.io.Writer
import javax.xml.stream.XMLStreamException
import util.Check

/**
* An interface that provides a means of processing an XML document.
* A Processor instance takes each node of the document and the previous state and
* produces the next state.  It also handles errors and the completion of the processing.
* The final result is of a common type, regardless of whether an error has occurred.  This
* common type could be Either[<some error type>, <a success type>], for example.
*
* 'process' may only be called once for a given XMLStream instance.
*/
trait XMLStream extends NotNull
{
	/** Basically, this is a fold on the nodes of the underlying XML document but with the
	* following behavior on encountering an error (either a well-formedness error in the
	* document, or a generated error in Processor.process) or completion.  An error is
	* transformed by the appropriate error handler in Processor to a common result type
	* (typically Either[<error type>, <success type>]), which is then returned as the result
	* of this function.  Upon errorless completion, the Processor will transform the final
	* value to the common result type.
	*/
	def process[S,PE,R](initialValue: S, processor: Processor[S,PE,R]): R
}
/** The interface for iterating over the XMLStream's nodes.*/
trait Processor[StateType, ProcessingErrorType, ResultType] extends NotNull
{
	/** Processes an error occurring from the underlying XML not being well-formed.*/
	def wellFormednessError(e: XMLStreamException): ResultType
	/** Processes an error occurring when a processed node is invalid (according to
	* the process function. */
	def processingError(e: ProcessingErrorType): ResultType
	
	/** Processes the final value. */
	def complete(finalValue: StateType): ResultType
	
	/** Processes the next node in the document given the state from processing the
	* previous node.  The result should be Left if there is an error processing
	* or Right if there was not. */
	def process(workingValue: StateType, node: Node): Either[ProcessingErrorType, StateType]
}

/**
* Basic information for an XML node.
*/
sealed trait Node extends NotNull
{
	import javax.xml.stream.Location
	
	/** The location of this node in the document. */
	def getLocation: Location
	
	/** A human readable description of this node */
	def description: String
	
	/**
	* Returns the current node similar to how it would appear in an XML document.
	* No escaping is done and element and attribute names are represented in
	* an expanded qualified name form ( {URI}localPart ).
	*/
	def toXMLLike: String
}

/** A text node. */
sealed trait Text extends Node
{
	/** Returns the value of this text node */
	def value: String
}

/** A processing instruction. */
sealed trait ProcessingInstruction extends Node
{
	/** The target of this processing instruction */
	def target: String
	
	/** The data of this processing instruction */
	def data: String
}

/** A comment */
sealed trait Comment extends Node
{
	/** The content of the comment. */
	def content: String
}

/**
* Common parent trait to nodes that occur within an element tag:
* Open, Close, Attribute, and EndTag
*
* Adds access to the element name in addition to the defined Node methods.
*/
sealed trait ElementTag extends Node
{
	/** Returns the qualified name for the current element. */
	def elementName: QName
}

/**
* Represents the end tag of an element.
*/
sealed trait EndTag extends ElementTag
/**
* Represents the beginning of the start tag of an element.  Zero
* or more attributes will follow this node, followed by a close
* node.
*/
sealed trait Open extends ElementTag
/**
* Represents the end of the start tag of an element.  This
* indicates that there are no further attributes for the
* current element.
*/
sealed trait Close extends ElementTag
/**
* Represents an attribute, providing access to the
* enclosing element name and the name and value of
* the attribute.
*/
sealed trait Attribute extends ElementTag
{
	/** The qualified name of this attribute. */
	def name: QName
	/** The text representation of the value of this attribute. */
	def value: String
}

/**
* A wrapper around the StAX2 API that presents the XML document
* as a stream of events.  Start tags of elements are decomposed
* into an opening event, an event for each attribute, and a
* closing event.
*
* Comments and processing instructions are currently discarded but
* should not be too hard to process.  The events
* and the information provided for each event are:
* Element start tag open (element qualified name)
* Element start tag close (element qualified name)
* Element end tag (element qualified name)
* Attribute (enclosing element qualified name, attribute qualified name, and attribute value)
* Text (value)
*
* The reader is the input source.  'process' may only be called once because it exhausts
* the underlying reader when called.
*/
import org.codehaus.stax2.XMLStreamReader2
private final class StAXStream(reader: XMLStreamReader2, config: StreamConfiguration) extends XMLStream
{
	import javax.xml.stream.XMLStreamConstants
	import javax.xml.stream.XMLStreamConstants._
	import net.frostbridge.util.TruncateString
	
	assume(reader != null, "XML Document stream cannot be null")
	verifyReaderIsFresh
	
	private def verifyReaderIsFresh
	{
		assume(reader.getEventType == START_DOCUMENT,
			"XML Document stream must be positioned at the beginning of the document.")
	}
	
	import StAXStream.nullToEmptyString
	
	def process[I,E,R](initialValue: I, processor: Processor[I,E,R]): R =
	{
		verifyReaderIsFresh
		
		try
		{
			processImpl(initialValue, processor) match
			{
				case Left(error) => processor.processingError(error)
				case Right(valid) => processor.complete(valid)
			}
		}
		catch
		{
			// underlying XML parser exception
			//  occurs when the XML is not well-formed
			case e: XMLStreamException => processor.wellFormednessError(e)
		}
		finally
		{
			reader.closeCompletely
		}
	}
	private[this] def processImpl[I,E,R](initialValue: I, processor: Processor[I,E,R]): Either[E,I] =
	{
		// pattern matching Either is done instead of _.right.flatMap(processElementContent)
		// because right.flatMap won't optimize tail recursion
		def processElementContent(state: I): Either[E,I] =
		{
			reader.next match
			{
				case END_ELEMENT =>
				{
					processor.process(state, end) match
					{
						case Right(newState) => processElementContent(newState)
						case error @ _ => error
					}
				}
				case START_ELEMENT =>
				{
					val elementName = getElementName
					val openTag = OpenImpl(elementName) :: getAttributes(elementName) ::: CloseImpl()(elementName) :: Nil
					openTag.foldLeft(Right(state): Either[E,I])(processWrapper) match
					{
						case Right(newState) => processElementContent(newState)
						case error @ _ => error
					}
				}
				case CHARACTERS if !(reader.isWhiteSpace && config.ignoreWhitespaceOnly) =>
				{
					processor.process(state, text) match
					{
						case Right(newState) => processElementContent(newState)
						case error @ _ => error
					}
				}
				case PROCESSING_INSTRUCTION if !config.ignoreProcessingInstructions =>
				{
					processor.process(state, processingInstruction) match
					{
						case Right(newState) => processElementContent(newState)
						case error @ _ => error
					}
				}
				case COMMENT if !config.ignoreComments =>
				{
					processor.process(state, comment) match
					{
						case Right(newState) => processElementContent(newState)
						case error @ _ => error
					}
				}
				case END_DOCUMENT => Right(state)
				case _ => processElementContent(state)
			}
		}
		
		def processWrapper(state: Either[E,I], node: Node): Either[E,I] = state.right.flatMap(processor.process(_, node))
		
		 // this will skip the document tag and other initial events before coming to the document element start tag
		 // the underlying parser will throw XMLStreamException if the document is not well formed, so we don't
		 // need to worry about that
		processElementContent(initialValue)
	}
	
	private sealed abstract class NodeImpl extends Node
	{
		val getLocation = reader.getLocation
	
		protected val depth: Int = reader.getDepth
		protected[this] def tabs = "\t" * depth
		
		// this is only valid in a subclass constructor, since it references the current state of the reader
		protected[this] final def require(eventType: Int) = reader.require(eventType, null, null)
		
		override def toString = toXMLLike
	}
	private def getElementName: QName =
	{
		val nsURI = reader.getNamespaceURI
		new QNameImpl(nullToEmptyString(nsURI), Check(reader.getLocalName))
	}
	private case class OpenImpl(elementName: QName) extends NodeImpl with ElementTag with Open
	{
		def toXMLLike = tabs + "<" + elementName.description + " "
		def description = "element '" + elementName.description + "'"
	}
	private case class CloseImpl()(val elementName: QName) extends NodeImpl with ElementTag with Close
	{
		def toXMLLike = ">"
		def description = "close of start tag for element '" + elementName.description + "'"
	}
	private def end: EndTag = EndImpl()(getElementName)
	private case class EndImpl()(val elementName: QName) extends NodeImpl with ElementTag with EndTag
	{
		def toXMLLike = tabs + "</" + elementName.description + ">"
		def description = "end tag for element '" + elementName.description + "'"
	}
	private case class AttributeImpl(name: QName, value: String)(val elementName: QName) extends NodeImpl with ElementTag with Attribute
	{
		def toXMLLike = keyValuePair
		def description = "attribute " + keyValuePair
		private[this] def keyValuePair = name.description + "=\"" + value + "\" "
	}
	
	private def attribute(index: Int, elementName: QName): Attribute =
	{
		reader.require(START_ELEMENT, null, null)
		assume(0 <= index && index < reader.getAttributeCount)
		val name =
		{
			val prefix = nullToEmptyString(reader.getAttributePrefix(index))
			val ns = nullToEmptyString(reader.getNamespaceURI(prefix))
			val localName = Check(reader.getAttributeLocalName(index))
			new QNameImpl(ns, localName)
		}
		val value = reader.getAttributeValue(index)
		AttributeImpl(name, value)(elementName)
	}
	/** Processes the current reader context into a list of attributes. The underlying
	* stream must be at an element start tag.*/
	private def getAttributes(elementName: QName): List[Attribute] =
	{
		reader.require(START_ELEMENT, null, null)
		val attributes =
			for(index <- (0 until reader.getAttributeCount)) yield
				attribute(index, elementName: QName)
		attributes.toList
	}
	/** Processes the current reader context into a text string. The underlying stream must
	* be at a position that can return text. */
	private def getText: String =
	{
		assume(reader.hasText)
		import java.io.StringWriter
		val writer = new StringWriter
		reader.getText(writer, false)
		writer.getBuffer.toString
	}
	private def text: Text =
	{
		reader.require(CHARACTERS, null, null)
		TextImpl(getText)
	}
	private case class TextImpl(value: String) extends NodeImpl with Text
	{
		def toXMLLike = value
		def description = "text '" + TruncateString(value) + "'"
	}
	private def comment: Comment =
	{
		reader.require(COMMENT, null, null)
		CommentImpl(getText)
	}
	private case class CommentImpl(content: String) extends NodeImpl with Comment
	{
		def description = "comment '" + TruncateString(content) + "'"
		def toXMLLike = "<!--" + content + "-->"
	}
	private def processingInstruction: ProcessingInstruction = 
	{
		reader.require(PROCESSING_INSTRUCTION, null, null)
		ProcessingInstructionImpl(reader.getPITarget, reader.getPIData)
	}
	private case class ProcessingInstructionImpl(target: String, data: String) extends NodeImpl with ProcessingInstruction
	{
		def toXMLLike = "<?" + target + " " + data + "?>"
		def description = "processing instruction {target='" + target + "' data='" + TruncateString(data) + "'}"
	}
}

/** Used to transform AttributePattern into NotAllowed pattern, so there is no location
* and the name can be arbitrary. */
private[frostbridge] class VirtualClose(name: String, val description: String) extends in.Close
{
	def toXMLLike = description
	def getLocation = com.ctc.wstx.io.WstxInputLocation.getEmptyLocation
	def elementName = QName("", name)
}

/** Configures the XMLStream to ignore certain XML nodes.  If ignoreWhitespaceOnly is true, text content that is
* entirely whitespace will be ignored.*/
final case class StreamConfiguration(ignoreProcessingInstructions: Boolean, ignoreComments: Boolean, ignoreWhitespaceOnly: Boolean)

/** Contains methods to create an XMLStream object using the Woodstox pull parser.*/
object StAXStream
{
	import java.net.URL
	import java.io.{File, InputStream, Reader}
	import DefaultInputFactory.createReader
	
	val DefaultStreamConfiguration = StreamConfiguration(true, true, true)
	
	/** Creates a new XMLStream from the given input file and with the specified configuration.*/
	def apply(file: File, config: StreamConfiguration): XMLStream = new StAXStream(createReader(file), config)
	/** Creates a new XMLStream from the given URL and with the specified configuration.*/
	def apply(url: URL, config: StreamConfiguration): XMLStream = new StAXStream(createReader(url), config)
	/** Creates a new XMLStream from the given InputStream and with the specified configuration.*/
	def apply(stream: InputStream, config: StreamConfiguration): XMLStream = new StAXStream(createReader(stream), config)
	/** Creates a new XMLStream from the given Reader and with the specified configuration.*/
	def apply(reader: Reader, config: StreamConfiguration): XMLStream = new StAXStream(createReader(reader), config)
	
	/** Creates a new XMLStream from the given input file using the default configuration of ignoring
	* comments, processing instructions, and whitespace only text content.*/
	def apply(file: File): XMLStream = apply(file, DefaultStreamConfiguration)
	/** Creates a new XMLStream from the given URL using the default configuration of ignoring
	* comments, processing instructions, and whitespace only text content.*/
	def apply(url: URL): XMLStream = apply(url, DefaultStreamConfiguration)
	/** Creates a new XMLStream from the given InputStream using the default configuration of ignoring
	* comments, processing instructions, and whitespace only text content.*/
	def apply(stream: InputStream): XMLStream = apply(stream, DefaultStreamConfiguration)
	/** Creates a new XMLStream from the given Reader using the default configuration of ignoring
	* comments, processing instructions, and whitespace only text content.*/
	def apply(reader: Reader): XMLStream = apply(reader, DefaultStreamConfiguration)
	
	/** Converts the given String to a String with NotNull by first checking that it is not null
	* and then casting.*/
	private def nullToEmptyString(value: String): String with NotNull =
		if(value == null)
			""
		else
			value.asInstanceOf[String with NotNull]
}
