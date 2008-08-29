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

import in.Node
import PatternImpl._
import PatternFactory._
import Optimize._

sealed trait Optimize
{
	def intern[Generated](p: Pattern[Generated]): Pattern[Generated]
	def derive[Generated](pattern: Pattern[Generated], node: Node): Pattern[Generated]
	def reduce[Generated](p: Pattern[Generated]): Pattern[Generated]
}

final class NoOptimize extends Optimize
{
	def intern[Generated](p: Pattern[Generated]) = p
	def derive[Generated](pattern: Pattern[Generated], node: Node) = pattern.deriveImpl(node)(this)
	def reduce[Generated](p: Pattern[Generated]) = p
}

final class InternedDeriveOptimize extends InternDeriveOptimize
{
	def reduce[Generated](p: Pattern[Generated]) = p
}
final class InternedFullOptimize extends InternDeriveOptimize with ReduceOptimize

trait ReduceOptimize extends Optimize
{
	var reduced: Int = _
	var minimal: Int = _
	var reducedAffected: Int = _
	
	import java.util.{IdentityHashMap,Map}
	def reduce[Generated](pattern: Pattern[Generated]): Pattern[Generated] =
	{
		pattern match
		{
			case h: HomogeneousChoice[_] => reduceHomogeneous(pattern, new IdentityHashMap)
			case _ => pattern
		}
	}
	private def reduceHomogeneous[Generated](pattern: Pattern[Generated], existing: Map[Pattern[Generated], Boolean]):
		Pattern[Generated] =
	{
		if(existing.put(pattern, true))
		{
			reduced += 1
			NotAllowedPattern
		}
		else
		{
			pattern match
			{
				case h: HomogeneousChoice[_] =>
				{
					val p1 = h.pattern1
					val p2 = h.pattern2
					val newPattern1 = reduceHomogeneous(p1, existing)
					val newPattern2 = reduceHomogeneous(p2, existing)
					if((newPattern1 eq p1) && (newPattern2 eq p2))
					{
						minimal += 1
						h
					}
					else
					{
						reducedAffected += 1
						homogeneousChoice(newPattern1, newPattern2)(this)
					}
				}
				case _ => pattern
			}
		}
	}
}

trait InternDeriveOptimize extends InternOptimize with DeriveOptimize
trait InternOptimize extends Optimize
{
	var requests: Int = _
	var maximumSize: Int = _
	import util.{CanonicalMap, WeakCanonicalMap}
	val internMap = new WeakCanonicalMap[Pattern[Any]]
	def intern[Generated](p: Pattern[Generated]): Pattern[Generated] =
	{
		requests += 1
		val result = internMap.asInstanceOf[CanonicalMap[Pattern[Generated]]].intern(p)
		maximumSize = maximumSize max internMap.size
		result
	}
}
trait DeriveOptimize extends Optimize
{
	import java.lang.ref.{Reference, SoftReference, WeakReference}
	import java.util.{HashMap, Map, WeakHashMap}
	private val deriveMap = new WeakHashMap[PatternWrapper,Map[Node,Reference[Any]]]
	
	var hits: Int = _
	var misses: Int = _
	val patternMissMap = new HashMap[String, java.lang.Integer]
	val patternHitMap = new HashMap[String, java.lang.Integer]
	val nodeMissMap = new HashMap[String, java.lang.Integer]
	val nodeHitMap = new HashMap[String, java.lang.Integer]
	
	private def key[Generated](pattern: Pattern[Generated]): String =
	{
		pattern match
		{
			case BasicTranslatingPattern(pattern, translator) => "Translate(" + pattern.getClass.getName + "," + translator.getClass.getName + ")"
			case _ => pattern.getClass.getName
		}
	}
	def derive[Generated](pattern: Pattern[Generated], node: Node): Pattern[Generated] =
	{
		def calculateDerive(map: Map[Node, Reference[Pattern[Generated]]]) =
		{
			misses += 1
			Optimize.increment(patternMissMap, key(pattern))
			Optimize.increment(nodeMissMap, node.getClass.getName)
			val calculated = pattern.deriveImpl(node)(this)
			map.put(node, new WeakReference(calculated))
			calculated
		}
	
		val wrapped = new PatternWrapper(pattern)
		val existingMap = deriveMap.get(wrapped)
		if(existingMap == null)
		{
			val newMap: Map[Node, Reference[Pattern[Generated]]] = new HashMap
			deriveMap.put(wrapped, newMap.asInstanceOf[Map[Node,Reference[Any]]])
			calculateDerive(newMap)
		}
		else
		{
			val nodeMap = existingMap.asInstanceOf[Map[Node, Reference[Pattern[Generated]]]]
			val previousResult =  nodeMap.get(node)
			if(previousResult == null)
				calculateDerive(nodeMap)
			else
			{
				val value = previousResult.get
				if(value == null)
					calculateDerive(nodeMap)
				else
				{
					hits += 1
			Optimize.increment(patternHitMap, key(pattern))
			Optimize.increment(nodeHitMap, node.getClass.getName)
					value
				}
			}
		}
	}
}

private class PatternWrapper(val p: Pattern[_])
{
	override def hashCode = p.hashCode
	override def equals(o: Any) =
	{
		o match
		{
			case w: PatternWrapper => w.p eq p
			case _ => false
		}
	}
}

object Optimize
{
	import java.util.Map
	def increment(map: Map[String, java.lang.Integer], key: String)
	{
		val count = map.get(key)
		if(count == null)
			map.put(key, 1)
		else
			map.put(key, count.intValue + 1)
	}
}