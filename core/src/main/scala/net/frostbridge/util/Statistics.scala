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
package net.frostbridge.util

abstract class Statistics
{ outer =>

	def elements: Int
	def attributes: Int
	def textNodes: Int
	def ordered: Int
	def notAllowed: Int
	def empty: Int
	def other: Int
	def elementContent: Int
	def repeat: Int
	def unordered: Int
	def choices: Int
	def heteroChoices: Int
	def translating: Int
	def comments: Int
	def processingInstructions: Int
	
	private def buildList: List[(String, Int)] =
		("Elements", elements) ::
		("Element content", elementContent) ::
		("Attributes", attributes) ::
		("Text nodes", textNodes) ::
		("Ordered", ordered) ::
		("Unordered", unordered) ::
		("Not allowed", notAllowed) ::
		("Empty", empty) ::
		("Choices", choices) ::
		("Mixed choice", heteroChoices) ::
		("Translating", translating) ::
		("Repeats", repeat) ::
		("Comments", comments) ::
		("Processing Instructions", processingInstructions) ::
		("Other", other) ::
		Nil
	
	private def mapList(l: List[(String, Int)]) = l.map(x => "\n  " + x._1 + ":\t" + x._2)
	private def compactFilter(x: (String, Int)): Boolean = x._2 != 0
	def toCompactString = mapList(buildList.filter(compactFilter)).mkString
	override def toString = buildList.map(x => "\n  " + x._1 + ":\t" + x._2).mkString(
		"\nStatistics\n" +
		"==========\n",
		"",
		"\n")
		
	def - (o: Statistics): Statistics =
		new Statistics
		{
			val elements = outer.elements - o.elements
			val attributes = outer.attributes - o.attributes
			val textNodes = outer.textNodes - o.textNodes
			val ordered = outer.ordered - o.ordered
			val notAllowed = outer.notAllowed - o.notAllowed
			val empty = outer.empty - o.empty
			val other = outer.other - o.other
			val elementContent = outer.elementContent - o.elementContent
			val repeat = outer.repeat - o.repeat
			val unordered = outer.unordered - o.unordered
			val choices = outer.choices - o.choices
			val heteroChoices = outer.heteroChoices - o.heteroChoices
			val translating = outer.translating - o.translating
			val comments = outer.comments - o.comments
			val processingInstructions = outer.processingInstructions - o.processingInstructions
		}
}

object Statistics
{
	import net.frostbridge._
	
	def statistics(pattern: Pattern[_]): Statistics =
	{
		val stats = new MutableStatistics
		statistics(pattern, stats)
		stats.toImmutable
	}
	private def statistics(pattern: Pattern[_], stats: MutableStatistics)
	{
		if(!stats.seenPattern(pattern))
		pattern match
		{
			case a: AttributePattern[_] => stats.attributes += 1
			case e: ElementPattern[_, _] => { stats.elements +=1; statistics(e.childrenPattern, stats) }
			case ec: ElementContentPattern[_, _] => { stats.elementContent +=1; statistics(ec.pattern, stats) }
			case o: OrderedSequence[_, _] => { stats.ordered += 1; statistics(o.pattern1, stats); statistics(o.pattern2, stats) }
			case g: UnorderedSequence[_, _] => { stats.unordered += 1; statistics(g.pattern1, stats); statistics(g.pattern2, stats) }
			case c: HomogeneousChoice[_] => { stats.choices += 1; statistics(c.pattern1, stats); statistics(c.pattern2, stats) }
			case h: HeterogeneousChoice[_, _] => { stats.heteroChoices += 1; statistics(h.pattern1, stats); statistics(h.pattern2, stats) }
			case t: TranslatingPattern[_, _] => { stats.translating += 1; statistics(t.delegate, stats) }
			case r: Repeat[_] => stats.repeat += 1; r.partial.map(p => statistics(p, stats)); statistics(r.repeated, stats)
			case v: EmptyPattern[_] => stats.empty += 1
			case t: TextPattern[_] => { stats.textNodes += 1 }
			case pi: ProcessingInstructionPattern[_] => { stats.processingInstructions += 1 }
			case c: CommentPattern[_] => { stats.comments += 1 }
			case NotAllowedPattern => stats.notAllowed += 1
			case _ => stats.other += 1
		}
	}
	
	private class MutableStatistics extends Statistics
	{
		private val seenPatterns = new java.util.HashSet[Pattern[_]]
		def seenPattern(p: Pattern[_]): Boolean = !seenPatterns.add(p)
		
		var elements, attributes, textNodes, ordered, notAllowed, empty, other, elementContent, repeat,
			unordered, choices, heteroChoices, translating, comments, processingInstructions: Int = _
		
		private def toTuple = (elements, attributes, textNodes, ordered, notAllowed, empty, other, elementContent, repeat,
			unordered, choices, heteroChoices, translating, comments, processingInstructions)
			
		def toImmutable: Statistics =
			new Statistics
			{
				val (elements, attributes, textNodes, ordered, notAllowed, empty, other, elementContent, repeat,
					unordered, choices, heteroChoices, translating, comments, processingInstructions) = toTuple
			}
	}
}