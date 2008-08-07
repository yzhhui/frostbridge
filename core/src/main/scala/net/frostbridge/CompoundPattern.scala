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

import TranslatingPattern.translate
import BinaryCompoundPattern._
import PatternImpl._
import Traceable._
import java.io.Writer

sealed trait BinaryCompoundPattern[A,B,C] extends UnmatchedPattern[C]
{
	type This = this.type
	def traceFlattened(writer: Writer, level: Int, reference: ReferenceFunction)
	{
		tracePattern(pattern1, writer, level, reference)
		basicTrace(writer, level, separator)
		tracePattern(pattern2, writer, level, reference)
		
	}
	def trace(writer: Writer, level: Int, reference: ReferenceFunction)
	{
		basicTrace(writer, level, "(")
		tracePattern(pattern1, writer, level, reference)
		basicTrace(writer, level, separator)
		tracePattern(pattern2, writer, level, reference)
		basicTrace(writer, level, ")")
	}
	private def tracePattern(pattern: Traceable, writer: Writer, level: Int, reference: ReferenceFunction)
	{
		if(isSameType(pattern))
			pattern.asInstanceOf[BinaryCompoundPattern[_,_,_]].traceFlattened(writer, level, reference)
		else
			pattern.embeddedTrace(writer, level+1, reference)
	}
	protected def isSameType(other: Traceable): Boolean
	
	def description = "( " + pattern1.description + " " + separator + " " + pattern2.description + " )"
	def pattern1: Pattern[A]
	def pattern2: Pattern[B]
	def separator: String
}
object BinaryCompoundPattern
{
	def homogeneousChoice[Generated](p1: Pattern[Generated], p2: Pattern[Generated]): Pattern[Generated] =
	{
		if(p1.valid)
		{
			if(p2.valid)
				new HomogeneousChoice(p1, p2)
			else
				p1
		}
		else
			p2
	}
	def heterogeneousChoice[A,B](p1: Pattern[A], p2: Pattern[B]): Pattern[Either[A, B]] =
	{
		if(p1.valid)
		{
			if(p2.valid)
				new HeterogeneousChoice(p1, p2)
			else
				translate(p1, Left(_: A), (_: Either[A,B]).left.toOption)
		}
		else
			translate(p2, Right(_: B), (_: Either[A,B]).right.toOption)
	}
	
	def unorderedSequence[A, B](p1: Pattern[A], p2: Pattern[B]) =
		requiredPair(p1, p2, (pA: Pattern[A], pB: Pattern[B]) => new UnorderedSequence(pA, pB))
	def orderedSequence[A, B](p1: Pattern[A], p2: Pattern[B]) =
		requiredPair(p1, p2, (pA: Pattern[A], pB: Pattern[B]) => new OrderedSequence(pA, pB))
	def requiredPair[A, B](p1: Pattern[A], p2: Pattern[B], constructor: (Pattern[A], Pattern[B]) => Pattern[(A, B)]): Pattern[(A, B)] =
	{
		p1.ifValid {
			p2.ifValid {
				p1.matched match
				{
					case Some(value1) =>
						p2.matched match
						{
							case Some(value2) => EmptyPattern( (value1, value2) )
							case None => translate(p2, (b: B) => (value1, b), (g: (A, B)) => Some(g._2) )
						}
					case None =>
						p2.matched match
						{
							case Some(value2) => translate( p1, (a: A) => (a, value2), (g: (A, B)) => Some(g._1) )
							case None => constructor(p1, p2)
						}
				}
			}
		}
	}

}
sealed trait BinaryRequired[A,B] extends BinaryCompoundPattern[A, B, (A,B)] with MarshalErrorTranslator[(A,B)]
{
	lazy val matchEmpty =
		for(value1 <- pattern1.matchEmpty; value2 <- pattern2.matchEmpty) yield
			(value1, value2)
			
	def marshal(g: (A, B), reverseXML: List[out.Node]) =
	{
		val (a, b) = g
		val dA = pattern1.marshal(a, reverseXML)
		val dB = pattern2.marshal(b, dA.right.getOrElse(Nil))
		val errors = List.lefts(dA :: dB :: Nil)
		if(errors.isEmpty)
			translateMarshalError(g)(dB)
		else
			Left(ChainedMarshalException(g, this)(errors))
	}
}

final class HeterogeneousChoice[A, B](val pattern1: Pattern[A], val pattern2: Pattern[B])
	extends BinaryCompoundPattern[A, B, Either[A,B]]
{
	checkAllowed(pattern1, pattern2)
	
	def separator = "|+|"
	def nextPossiblePatterns = pattern1.nextPossiblePatterns ::: pattern2.nextPossiblePatterns 
	
	def marshal(g: Either[A,B], reverseXML: List[out.Node]) =
		for(error <- g.fold(pattern1.marshal(_, reverseXML), pattern2.marshal(_, reverseXML)).left) yield
			ChainedMarshalException(g, this)(List(error))
	
	def derive(node: in.Node) = heterogeneousChoice(pattern1.derive(node), pattern2.derive(node))
	
	lazy val matchEmpty = pattern1.matchEmpty.map(Left(_)).orElse(pattern2.matchEmpty.map(Right(_)))
	
	protected def isSameType(other: Traceable) = other.isInstanceOf[HeterogeneousChoice[_,_]]
}

final class HomogeneousChoice[Generated](val pattern1: Pattern[Generated], val pattern2: Pattern[Generated])
	extends BinaryCompoundPattern[Generated,Generated,Generated]
{
	checkAllowed(pattern1, pattern2)
	
	def separator = "|"
	
	def nextPossiblePatterns = pattern1.nextPossiblePatterns ::: pattern2.nextPossiblePatterns 
	
	def derive(node: in.Node) = homogeneousChoice(pattern1.derive(node), pattern2.derive(node))
	
	lazy val matchEmpty =
	{
		val m = pattern1.matchEmpty
		if(m.isEmpty)
			pattern2.matchEmpty
		else
			m	// Forced determinism
	}
	
	def marshal(g: Generated, reverseXML: List[out.Node]) =
	{
		for(errorA <- pattern1.marshal(g, reverseXML).left;// Forced determinism
			errorB <- pattern2.marshal(g, reverseXML).left)
		yield
			ChainedMarshalException(g, this)(errorA :: errorB :: Nil)
	}
	
	protected def isSameType(other: Traceable) = other.isInstanceOf[HomogeneousChoice[_]]
}

final class OrderedSequence[A,B](val pattern1: Pattern[A], val pattern2: Pattern[B]) extends BinaryRequired[A,B]
{
	checkNonEmpty(pattern1, pattern2)
	checkAllowed(pattern1, pattern2)

	def derive(node: in.Node) =
	{
		node match
		{
			case attribute: in.Attribute =>
				(pattern1.derive(attribute) :+: pattern2) | (pattern1 :+: pattern2.derive(attribute))
			case close: in.Close =>
				pattern1.derive(close) :+: pattern2.derive(close)
			case _ =>
			{
				val derived = pattern1.derive(node) :+: pattern2
				pattern1.matchEmpty match
				{
					//pattern2 -> pattern2.derive(node) 12July2008
					case Some(value) => derived | (EmptyPattern(value) :+: pattern2.derive(node))
					case None => derived
				}
			}
		}
	}
	
	def nextPossiblePatterns =
	{
		val p1Possible = pattern1.nextPossiblePatterns
		if(pattern1.matchEmpty.isEmpty)
			p1Possible
		else
			p1Possible ::: pattern2.nextPossiblePatterns
	}
	def separator = ":+:"
	
	protected def isSameType(other: Traceable) = other.isInstanceOf[OrderedSequence[_,_]]
}

final class UnorderedSequence[A,B](val pattern1: Pattern[A], val pattern2: Pattern[B]) extends BinaryRequired[A,B]
{
	checkNonEmpty(pattern1, pattern2)
	checkAllowed(pattern1, pattern2)
	
	def derive(node: in.Node) =
	{
		node match
		{
			case _: in.Close =>
				pattern1.derive(node) +++ pattern2.derive(node)
			case attribute: in.Attribute =>
				(pattern1.derive(node) +++ pattern2) |
					translate(pattern2.derive(node) +++ pattern1, (p: (B, A) ) => p.swap, (p: (A, B)) => Some(p.swap))
			case _ =>
				(pattern1.derive(node) :+: pattern2) | translate(pattern2.derive(node) :+: pattern1,
					(p: (B, A) ) => p.swap, (p: (A, B)) => Some(p.swap))
		}
	}
	
	
	def nextPossiblePatterns = pattern1.nextPossiblePatterns ::: pattern2.nextPossiblePatterns 
	
	def separator = ","
	
	protected def isSameType(other: Traceable) = other.isInstanceOf[UnorderedSequence[_,_]]
}

