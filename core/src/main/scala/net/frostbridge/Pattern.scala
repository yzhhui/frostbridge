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

/*
* Defines what XML nodes are expected and how to generate an object from the matched XML.
*
* The different patterns are:
*	Element
*	Attribute
*	Element content (internal use only)
*	Choice (Homogeneous or Heterogeneous)
*	Sequence (Ordered or Unordered)
*	Repeat
*	Text
*	Empty
*	Not allowed
*
* The possible types of nodes are:
*	Start tag open
*	Attribute
*	Start tag close
*	Text
*	End tag
*/
trait Pattern[Generated] extends Traceable with NotNull
{
	/** Makes the Generated type parameter accessible to clients. */
	final type GeneratedType = Generated
	
	/**
	* Handles the given XML node by returning the next pattern to match.
	* If the node does not match, NotAllowedPattern is returned.
	*/
	def derive(node: in.Node): Pattern[Generated]
	
	/**
	* Returns None if this value does not match the empty sequence, otherwise
	* returning the value that this pattern generates when matching the empty
	* sequence.
	*/
	def matchEmpty: Option[Generated]
	
	/**
	* Returns the value matched or None if this pattern has not been completely
	* matched yet.  Only EmptyPattern should return anything here.
	*/
	def matched: Option[Generated]
	
	/**
	* True if and only if this pattern is valid (that is, it is not an error)
	*/
	def valid: Boolean = true
	
	/**
	* If this pattern is valid, this function returns the value of f.
	* Otherwise, this function returns NotAllowedPattern.
	*/
	def ifValid[NewGenerated](f: => Pattern[NewGenerated]): Pattern[NewGenerated] =
	{
		import PatternImpl.translateNotAllowed
		if(valid)
			f
		else
			NotAllowedPattern
	}
	
	/**
	* Constructs XML nodes from a generated object 'g' and prepends them to the reverse list
	* of XML nodes in 'reverseXML'.  'reverseXML' should be reversed before consumption.
	*/
	def marshal(g: Generated, reverseXML: List[out.Node]): Either[MarshalException[Generated], List[out.Node]]


	import Repeat.{optional, repeat}
	
	/**
	* Produces a pattern that matches this pattern one or more times.
	*/
	def + : Pattern[List[Generated]] = repeat(this, 1, Infinite)
	
	/**
	* Produces a pattern that matches this pattern zero or more times.
	*/
	def * : Pattern[List[Generated]] = repeat(this, 0, Infinite)
	
	/**
	* Produces a pattern that matches this pattern once or not at all.
	*/
	def ? : Pattern[Option[Generated]] = optional(this)
	
	/**
	* Produces a pattern that matches this pattern at least min times and at most max times.
	*/
	def apply(min: Int, max: UpperBound): Pattern[List[Generated]] = repeat(this, min, max)
	
	import BinaryCompoundPattern.{orderedSequence, unorderedSequence, heterogeneousChoice, homogeneousChoice}
	
	/**
	* Produces a pattern that matches firstPattern and then this pattern in order.  Attributes
	* are matched unordered.  Note that this is right associative.
	*/
	def :+: [GeneratedOther](firstPattern: Pattern[GeneratedOther]): Pattern[(GeneratedOther, Generated)] =
		orderedSequence(firstPattern, this)
	/**
	* Produces a pattern that matches this pattern and then secondPattern or secondPattern and then
	* this pattern.  Attributes are matched unordered.  Note that this is left associative.
	*/
	def +++ [GeneratedOther](secondPattern: Pattern[GeneratedOther]): Pattern[(Generated, GeneratedOther)] =
		unorderedSequence(this,secondPattern)
		
	/**
	* Produces a pattern that matches otherPattern or this pattern.  The result is Left if this pattern matched
	* or Right if otherPattern matched.  If both match, the result is Left.
	*/
	def |+| [GeneratedOther](otherPattern: Pattern[GeneratedOther]): Pattern[Either[Generated,GeneratedOther]] =
		heterogeneousChoice(this, otherPattern)
	/**
	* Produces a pattern that matches otherPattern or this pattern.
	*/
	def | (otherPattern: Pattern[Generated]): Pattern[Generated] =
		homogeneousChoice(this, otherPattern)
}
