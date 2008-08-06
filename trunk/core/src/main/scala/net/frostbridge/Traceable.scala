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

import Traceable._

import scala.collection.jcl.SortedMap
import scala.collection.mutable.Map

import java.io.Writer

trait Traceable extends NotNull
{
	/*
	* Returns a list of the possible next expected patterns, which may be
	* text, element, or attribute patterns
	*/
	def nextPossiblePatterns: List[Traceable]
	
	def startTrace(writer: Writer)
	{
		import scala.collection.mutable.HashMap
		import scala.collection.jcl.TreeMap
		
		val newPatterns: SortedMap[String, Traceable] = new TreeMap[String, Traceable]
		val referencedPatterns: Map[Traceable, String] = new HashMap[Traceable, String]
		val referencePatternFunction = referencePattern(referencedPatterns, newPatterns, _: Traceable)
		
		val name = referencePatternFunction(this)
		writer.write("start = " + name + "\n")
		writer.write(name + " = ")
		trace(writer, 0, referencePatternFunction)
		
		newPatterns -= name
		
		while(!newPatterns.isEmpty)
		{
			val name = newPatterns.firstKey
			val pattern = newPatterns(name)
			newPatterns -= name
			
			writer.write(name + " = ")
			pattern.trace(writer, 0, referencePatternFunction)
		}
	}
	
	def description: String
	
	/*
	* Writes a complete (deep) representation of this object to the given writer.
	* The name of patterns to include by reference should be obtained through the
	* reference function.
	*/
	def trace(writer: Writer, level: Int, reference: ReferenceFunction)
	
	
	def embeddedTrace(writer: Writer, level: Int, reference: ReferenceFunction) = trace(writer, level, reference)
}

trait ReferencedTraceable extends Traceable
{
	def name: String
	
	override def embeddedTrace(writer: Writer, level: Int, referenceFunction: ReferenceFunction)
	{
		if(level == 0)
			trace(writer, level, referenceFunction)
		else
			basicTrace(writer, level, referenceFunction(this))
	}
}

object Traceable
{
	def trace(pattern: Traceable)
	{
		val writer = new java.io.OutputStreamWriter(System.out)
		pattern.startTrace(writer)
		writer.flush
	}

	type ReferenceFunction = Traceable => String
	
	def basicTrace(writer: Writer, level: Int, text: String) =
	{
		writeLevel(writer, level)
		writer.write(text)
		writer.append('\n')
	}
	def writeLevel(writer: Writer, level: Int)
	{
		if(level > 0)
		{
			writer.write("    ")
			writeLevel(writer, level - 1)
		}
	}
	
	private[this] def referencePattern(referencedPatterns: Map[Traceable, String], newPatterns: Map[String, Traceable], traceable: Traceable): String =
	{	
		def validReferencedTraceable(r: ReferencedTraceable) = 
		{
			if(r.name == null)
				false
			else
			{
				val boundName = referencedPatterns.getOrElse(r, null)
				boundName == null || boundName == r.name
			}
		}
		
		referencedPatterns.get(traceable) match
		{
			case Some(value) => value
			case None =>
			{
				val mappedName = traceable match
				{
					case r: ReferencedTraceable if validReferencedTraceable(r) => r.name
					case _ => "Pattern" + (referencedPatterns.size + 1)
				}
				referencedPatterns += ((traceable, mappedName))
				newPatterns += ((mappedName, traceable))
				mappedName
			}
		}
	}
}