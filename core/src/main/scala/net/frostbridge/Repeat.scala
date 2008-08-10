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

import PatternImpl._
import Traceable.{basicTrace, ReferenceFunction}
import PatternFactory._
import java.io.Writer

/**
* Matches a pattern between min and max times, inclusive (although max = Infinite
* is technically exclusive)
* This class is optimized so that the stack doesn't overflow when matching a long sequence.
* It represents
*	partial :+: (repeated {min, max})
* in a way that accumulates matched values in reverse in a List instead of piling up
* TranslatedPatterns that generate the list on a call to matchEmpty (which will overflow
* the stack for several thousand values in a sequence)
*/
private final class Repeat[Generated]
	(val partial: Option[Pattern[Generated]],
	 val repeated: Pattern[Generated],
	 val accumulatedReverse: List[Generated],
	 val min: Int, val max: UpperBound)
	extends UnmatchedPattern[List[Generated]] with MarshalErrorTranslator[List[Generated]]
{
	partial.foreach(checkNonEmpty)
	partial.foreach(checkAllowed)
	checkNonEmpty(repeated)
	checkAllowed(repeated)
	assume(0 <= min, "Minimum occurences must be non-negative")
	assume(max >= min, "Minimum occurences must be less than the maximum occurences")
	
	def separator = ":+:"
	
	def derive(node: in.Node) =
	{
		partial match
		{
			case None => repeatDerive(node, None)
			case Some(partialPattern) =>
			{
				node match
				{
					case attribute: in.Attribute =>
						repeat(partialPattern.derive(attribute), repeated, accumulatedReverse, min, max)// |
						//TODO: repeat(partialPattern :+: repeated.derive(attribute), repeated, accumulatedReverse, min, max)
					case close: in.Close =>
						repeat(partialPattern.derive(close), repeated.derive(close), accumulatedReverse, min, max)
					case _ =>
					{
						val derived = repeat(partialPattern.derive(node), repeated, accumulatedReverse, min, max)
						partialPattern.matchEmpty match
						{
							case Some(value) => derived | repeatDerive(node, Some(value))
							case None => derived
						}
					}
				}
			}
		}
	}
	private def repeatDerive(node: in.Node, prependValue: Option[Generated]): Pattern[List[Generated]] =
	{
		reducedBounds match
		{
			case Some((newMin, newMax)) =>
				repeat(repeated.derive(node), repeated, prependValue.toList ::: accumulatedReverse, newMin, newMax)
			case None => translateLast(repeated.derive(node), accumulatedReverse)
		}
	}
	
	private val reducedBounds: Option[(Int, UpperBound)] =
	{
		if(max.isOne)
			None
		else
			Some(( if(min == 0) 0 else (min - 1), max.decrement ))
	}
	
	lazy val matchEmpty =
	{
		val partialEmptyOption =
			partial match
			{
				case None => Some(Nil)
				case Some(partialPattern) => for(value <- partialPattern.matchEmpty) yield (value :: Nil)
			}
		for(partialEmpty <- partialEmptyOption; repeatEmpty <- repeatedMatchEmpty) yield
			accumulatedReverse reverse_::: partialEmpty ::: repeatEmpty
	}
	private def repeatedMatchEmpty =
	{
		if(min == 0)
			Some(Nil)
		else
			// forced determinism
			for(value <- repeated.matchEmpty) yield
				List.make(min, value)
	}
	
	def nextPossiblePatterns =
	{
		partial match
		{
			case None => repeatNextPossiblePatterns
			case Some(partialPattern) =>
			{
				val partialPossible = partialPattern.nextPossiblePatterns
				if(partialPattern.matchEmpty.isEmpty)
					partialPossible
				else
					partialPossible ::: repeatNextPossiblePatterns
			}
		}
	}
	private def repeatNextPossiblePatterns = repeated.nextPossiblePatterns

	
	def marshal(g: List[Generated], reverseXML: List[out.Node]) =
	{
		if(partial.isDefined || !accumulatedReverse.isEmpty)
			error("Cannot marshal a partially applied repeating pattern.")
		else
			repeatMarshal(g, reverseXML)
	}
	
	private def repeatMarshal(list: List[Generated], reverseXML: List[out.Node]) =
	{
		if(validLength(list.size))
		{
			val initial: Either[MarshalException[Generated], List[out.Node]] = Right(reverseXML)
			translateMarshalError(list)(list.foldLeft(initial)(foldMarshal))
		}
		else
			Left(RootMarshalException(list, this))
	}
	private def validLength(length: Int): Boolean = max >= length && length >= min
	
	private def foldMarshal(result: Either[MarshalException[Generated], List[out.Node]], g: Generated):
		Either[MarshalException[Generated], List[out.Node]] =
	{
		result.right.flatMap(newXML => repeated.marshal(g, newXML))
	}
	
	def description =
		partial match
		{
			case Some(p1) => "( " + p1.description + " " + separator + " " + repeatDescription + " )"
			case None =>  repeatDescription
		}
	private def repeatDescription = "( " + repeated.description + " )" + generateRepetitionClause
	private def generateRepetitionClause =
	{
		max match
		{
			case Finite(value) =>
			{
				if(value == 1 && min == 0)
					"?"
				else
					"{" + min + "," + value + "}"
			}
			case Infinite =>
			{
				if(min == 0)
					"*"
				else if(min == 1)
					"+"
				else
					"{" + min + ",}"
			}
		}
	}
	
	def trace(writer: Writer, level: Int, reference: ReferenceFunction)
	{
		partial match
		{
			case Some(partialPattern) =>
			{
				basicTrace(writer, level, "(")
				partialPattern.embeddedTrace(writer, level+1, reference)
				basicTrace(writer, level, separator)
				repeatTrace(writer, level, reference)
				basicTrace(writer, level, ")")
			}
			case None => repeatTrace(writer, level, reference)
		}
	}
	
	private def repeatTrace(writer: Writer, level: Int, reference: ReferenceFunction)
	{
		basicTrace(writer, level, "(")
		repeated.embeddedTrace(writer, level+1, reference)
		basicTrace(writer, level, ")" + generateRepetitionClause)
	}
}

trait RepeatPatternFactory
{
	final def repeat[G](repeated: Pattern[G], min: Int, max: UpperBound): Pattern[List[G]] =
		repeat(None, repeated, Nil, min, max) 
	private[frostbridge] final def repeat[G](partial: Pattern[G], repeated: Pattern[G], accumulatedReverse: List[G],
		min: Int, max: UpperBound): Pattern[List[G]] =
			repeat(Some(partial), repeated, accumulatedReverse, min, max)
	private[frostbridge] final def repeat[G](partial: Option[Pattern[G]], repeated: Pattern[G], accumulatedReverse: List[G],
		 min: Int, max: UpperBound): Pattern[List[G]] =
	{
		assume(min >= 0, "Minimum must be greater than or equal to zero")
		
		def checkRepeated(invalidButOptional: => Pattern[List[G]]): Pattern[List[G]] =
		{
			if(!repeated.valid)
			{
				if(min == 0)
					invalidButOptional
				else
					NotAllowedPattern
			}
			else
			{
				repeated.matched match
				{
					case Some(value) => emptyPattern(List(value))
					case None => new Repeat(partial, repeated, accumulatedReverse, min, max)
				}
			}
		}
		
		partial match
		{
			case Some(partialPattern) =>
				partialPattern.ifValid
				{
					partialPattern.matched match
					{
						case Some(value) => repeat(None, repeated, value :: accumulatedReverse, min, max)
						case None => checkRepeated(translateLast(partialPattern, accumulatedReverse))
					}
				}
			case None =>
				checkRepeated(emptyPattern(Nil))
		}
	}
	
	private[frostbridge] def translateLast[Generated](pattern: Pattern[Generated], accumulatedReverse: List[Generated]) =
		translate(pattern, (g: Generated) => accumulatedReverse reverse_::: g :: Nil,
			(l: List[Generated]) => l.firstOption)
			
	final def optional[G](pattern: Pattern[G]): Pattern[Option[G]] =
		emptyPattern[Option[G]](None) | translate(pattern, (g: G) => Some(g), (s: Option[G]) => s)
}
sealed trait UpperBound extends NotNull
{
	def >=(min: Int): Boolean
	def isOne: Boolean
	def decrement: UpperBound
	def isInfinite: Boolean
}
case object Infinite extends UpperBound
{
	def >=(min: Int) = true
	def isOne = false
	def decrement = this
	def isInfinite = true
	override def toString = "Infinity"
}
final case class Finite(val value: Int) extends UpperBound
{
	assume(value > 0, "Maximum occurences must be greater than zero")
	
	def >=(min: Int) = value >= min
	def isOne = value == 1
	def decrement = Finite(value - 1)
	def isInfinite = false
	override def toString = value.toString
}