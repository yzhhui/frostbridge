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

/** An object that can write a representation of itself to a Writer. */
trait Traceable extends NotNull
{
	/**
	* Returns a list of the possible next expected patterns, which may be
	* text, element, comment, processing instruction, or attribute patterns.
	*/
	def nextPossiblePatterns: List[Traceable]
	
	/** Writes a representation of this object to the given writer with this object
	* as the top level. */
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
	
	/**
	* Writes a complete representation of this object to the given writer.
	* When an object is written as a reference, it should obtain its reference
	* name from the reference function.
	*/
	private[frostbridge] def trace(writer: Writer, level: Int, reference: ReferenceFunction)
	
	/** Writes a representation of this object for the case where it is enclosed in another object.
	* The default implementation is the same as 'trace'.*/
	private[frostbridge] def embeddedTrace(writer: Writer, level: Int, reference: ReferenceFunction) =
		trace(writer, level, reference)
}

/** Represents a Traceable that may be referenced.  This means that when tracing out an enclosing object, that
* object may write a reference to this object instead of the full contents of this object.  In this case,
* this object should be appended to the trace after the enclosing object is traced.  For example:
* element url
* {
*    attribute asdf { text }
*    Pattern location
* }
* location = element location
* {
*   anyURL
* }
*
* Here, location is referenced by name in the trace of element url and expanded after url is completed.
* This allows for a more readable trace and for traces of cyclic objects.
*/
trait ReferencedTraceable extends Traceable
{
	def name: String
	
	private[frostbridge] override def embeddedTrace(writer: Writer, level: Int, referenceFunction: ReferenceFunction)
	{
		if(level == 0)
			trace(writer, level, referenceFunction)
		else
			basicTrace(writer, level, referenceFunction(this))
	}
}

/** Helper functions 
*/
object Traceable
{
	def trace(pattern: Traceable)
	{
		val writer = new java.io.OutputStreamWriter(System.out)
		pattern.startTrace(writer)
		writer.flush
	}
	def traceToString(pattern: Traceable): String =
	{
		val writer = new java.io.StringWriter
		pattern.startTrace(writer)
		writer.toString
	}

	type ReferenceFunction = Traceable => String
	
	private[frostbridge] def basicTrace(writer: Writer, level: Int, text: String) =
	{
		writeLevel(writer, level)
		writer.write(text)
		writer.append('\n')
	}
	private[frostbridge] def writeLevel(writer: Writer, level: Int)
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