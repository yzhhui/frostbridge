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

import javax.xml.stream.XMLStreamException

/** An instance of this trait is provided to Unmarshaller to customize handling of
* the unmarshalling process.
*/
trait UnmarshalHandler[Generated, ResultType] extends NotNull
{
	/** This is called when the source XML is found to be invalid at a certain node. */
	def invalidNode(lastGoodPattern: Pattern[Generated], troubleNode: in.Node): ResultType
	
	/** This is called when the source document is processed without error, but the
	* pattern expects more nodes.
	*/
	def incompleteMatch(unmatchedPattern: Pattern[Generated]): ResultType
	
	/** This is called when the underlying XML parser indicates that the XML is not
	* well-formed.
	*/
	def wellFormednessError(e: XMLStreamException): ResultType
	
	/** Called when the unmarshal process is completely successful and has produced
	* the given object.
	*/
	def unmarshalled(g: Generated): ResultType
}

/** An implementation of UnmarshalHandler that produces an Either object as the result.
* It converts the various error conditions to an error String in a Left object
* or returns the generated object in a Right object.
*/
class BasicHandler[T] extends UnmarshalHandler[T, Either[String, T]]
{
	def invalidNode(lastGoodPattern: Pattern[T], troubleNode: in.Node) =
	{
		Left("Validation Error:\n" + 
		Unmarshaller.expectedPatterns(lastGoodPattern) +
		"Found\n\t" + troubleNode.description +
		"\n at " + troubleNode.getLocation + "\n")
	}
	
	def incompleteMatch(unmatchedPattern: Pattern[T]) =
		Left(Unmarshaller.expectedPatterns(unmatchedPattern) + "Found\n\tend of document\n")
	
	def wellFormednessError(e: XMLStreamException) =
		Left("Well-formedness error: " + e.getMessage +
		"\n at " + e.getLocation + "\n")
	
	def unmarshalled(g: T) = Right(g)
}

object Unmarshaller
{
	/** Unmarshals the given document according to the given pattern using 
	* a BasicHandler.  The result will be the generated value in Right or
	* an error message string in Left.
	*/
	def unmarshalOrError[Generated](pattern: Pattern[Generated], document: in.XMLStream) =
		(new Unmarshaller(pattern, new BasicHandler[Generated])).unmarshal(document)
	
	/**
	* Shows the patterns that were expected to be matched (usually called after an
	* invalid token is handled).
	*/
	def expectedPatterns(lastKnownGoodPattern: Pattern[_]): String =
	{
		val possibilities = lastKnownGoodPattern.nextPossiblePatterns
		
		val base =
		"Expected " +
			{
				if(possibilities.size == 0)
				{
					error("No possible next patterns found in: " + lastKnownGoodPattern.description)
					""
				}
				else if(possibilities.size > 1)
					"one of:\n"
				else
					"\n"
			}
		possibilities.map("\t" + _.description).mkString(base, "\n", "\n")
	}
}

/**
* This class is used to unmarshal an XML document according to a pattern.
* The pattern model is similar to the RelaxNG model.  Notable differences are the ability
* to specify the minimum and maximum times something can be repetitively matched (RelaxNG
* provides 0-infinity, 0-1, 1-infinity) and forced determinism in certain cases (RelaxNG only
* validates a document, so two alternatives can match and the document is valid.  When
* binding, only one alternative can be bound.)
*/
final class Unmarshaller[Generated, ResultType]
	(pattern: Pattern[Generated], handler: UnmarshalHandler[Generated, ResultType]) extends NotNull
{
	private case class UnmarshalError[Generated](lastGoodPattern: Pattern[Generated], troubleNode: in.Node)
	
	private object NodeProcessor
		extends in.Processor[Pattern[Generated], UnmarshalError[Generated], ResultType]
	{
		/*private var statistics = util.Statistics.statistics(pattern)
		println("Initial statistics:\n" + statistics.toCompactString + "\n")*/
		def process(currentPattern: Pattern[Generated], node: in.Node) =
		{
			val newPattern = currentPattern.derive(node)
			if(newPattern.valid)
			{
				/*val oldStatistics = statistics
				statistics = util.Statistics.statistics(newPattern)
				println("Statistics change:\n" + (statistics - oldStatistics).toCompactString + "\n")*/
				Right(newPattern)
			}
			else
				Left(UnmarshalError(currentPattern, node))
		}
		def processingError(e: UnmarshalError[Generated]) = handler.invalidNode(e.lastGoodPattern, e.troubleNode)
		def wellFormednessError(e: XMLStreamException) = handler.wellFormednessError(e)
		def complete(result: Pattern[Generated]) =
		{
			//no error if the document has been completely parsed
			result.matchEmpty match
			{
				case Some(generated) => handler.unmarshalled(generated)
				case None => handler.incompleteMatch(result)
			}
		}
	}
	
	/** Performs the unmarshalling. */
	final def unmarshal(document: in.XMLStream) = document.process(pattern, NodeProcessor)
}