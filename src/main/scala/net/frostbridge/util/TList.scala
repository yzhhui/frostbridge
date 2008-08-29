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

/** A linked list that aims to implement methods in a tail-recursive fashion and cache the length
* and hashCode of the list.*/
sealed abstract class TList[A] extends Seq[A] with NotNull
{
	/** Prepends value to this list. */
	def ::(value: A): TList[A]
	/** Reverses the order of this list. */
	override def reverse: TList[A] = TList.empty // must be concrete to override the type defined in Seq
	/** Prepends the given list in reverse order (equivalent to prepend.reverse ::: this but more efficient).*/
	def reverse_:::(prepend: TList[A]): TList[A]
	override protected def stringPrefix = "TList"
}

private object Empty extends EmptyTList[Any]
private sealed class EmptyTList[A] extends TList[A]
{
	def apply(index: Int) = throw new IndexOutOfBoundsException(index.toString)
	def length = 0
	def elements: Iterator[A] = Iterator.empty
	def ::(newValue: A): TList[A] = TList(newValue)
	override def toString = "TList()"
	override def reverse: TList[A] = this
	def reverse_:::(prepend: TList[A]) = prepend.reverse
}
/** Represents a tail node in the list.  This is a simple container for the current
* node value and the next node in the list.*/
private final class Tail[T](val value: T, val next: Option[Tail[T]]) extends NotNull
{
	private[frostbridge] def apply(index: Int): T =
	{
		if(index == 0)
			value
		else
 			next.get.apply(index - 1)
	}
	
	def toString(buffer: StringBuilder)
	{
		buffer.append(", ")
		buffer.append(value)
		next match
		{
			case Some(n) => n.toString(buffer)
			case None => ()
		}
	}
}
/** Implements a non-empty TList.  The length and hash of the list are cached.
* This class will only exist as the head of a list.  When either prepend method
* is called on this class, the new list that is returned uses a Tail in place of
* this Head.*/
private final class Head[A](val value: A, val next: Option[Tail[A]], val length: Int, val hash: Int) extends TList[A]
{
	assume(length > 0)
	
	import TList._
	
	def apply(index: Int) =
	{
		if(index == 0)
			value
		else if(index >= length || index < 0)
			throw new IndexOutOfBoundsException(index.toString)
		else
			next.get.apply(index - 1)
	}
	override def reverse: TList[A] = TList.reverse(next, TList(value))
	def reverse_:::(prepend: TList[A]) = prepend.elements.foldLeft(this: TList[A])( (list: TList[A], value: A) => value :: list )
	def ::(newValue: A): TList[A] = new Head(newValue, Some(new Tail(value, next)), length + 1, newHash(newValue, hash))
	
	override def equals(a: Any): Boolean =
	{
		a match
		{
			// All that is strictly necessary is (this sameElements h).  The others are cheap shortcuts.
			case h: Head[_] => (h eq this) || (h.hash == this.hash && h.length == this.length && (this sameElements h))
			case _ => false
		}
	}
	override def hashCode =
	{
		val hash1 = hash + (hash << 3)
		val hash2 = hash1 ^ (hash1 >> 11)
		hash2 + (hash2 << 15)
	}
	override def toString = mkString("TList(", ", ", ")")
	def elements: Iterator[A] = new TListIterator[A](this)
}

/** Iterator for a TList. */
private final class TListIterator[T](head: Head[T]) extends Iterator[T]
{
	private var nextTail: Option[Tail[T]] = Some(new Tail(head.value, head.next))
	
	def hasNext: Boolean = nextTail.isDefined
	def next: T =
	{
		nextTail match
		{
			case Some(tail) =>
			{
				nextTail = tail.next
				tail.value
			}
			case None => throw new java.util.NoSuchElementException
		}
	}
}

object TList
{
	/** The empty list.*/
	def empty[T]: TList[T] = Empty.asInstanceOf[EmptyTList[T]]
	/** Creates a list of length one consisting of the given value.*/
	def apply[T](value: T): TList[T] = new Head(value, None, 1, newHash(value, 0))
	
	/** Creates a list containing the values between start and end (inclusive) in increasing order.
	* start must be <= end.*/
	def range(start: Int, end: Int): TList[Int] =
	{
		assume(start <= end)
		prependRange(empty, start, end)
	}
	private def prependRange(a: TList[Int], start: Int, end: Int): TList[Int] =
	{
		if(start > end)
			a
		else
			prependRange(end :: a, start, end - 1)
	}
	
	/** Creates a list of length n consisting of the given value. */
	def make[A](n: Int, value: A): TList[A] =
	{
		assume(n >= 0)
		make(n, value, empty)
	}
	private def make[A](n: Int, value: A, list: TList[A]): TList[A] =
	{
		if(n == 0)
			list
		else
			make(n-1, value, value :: list)
	}
	
	private[frostbridge] final def reverse[T](tail: Option[Tail[T]], reversed: TList[T]): TList[T] =
	{
		if(tail.isDefined)
		{
			val t = tail.get
			reverse(t.next, t.value :: reversed)
		}
		else
			reversed
	}
	
	// Bob Jenkins' One-at-a-Time Hash from http://burtleburtle.net/bob/hash/doobs.html
	// (public domain)
	final def newHash[T](newValue: T, oldHash: Int): Int = 
	{
		val newValueHash = newValue.hashCode
		val newHash1 = iterateHash(newValueHash & 0xFF, oldHash)
		val newHash2 = iterateHash((newValueHash >>> 8) & 0xFF, newHash1)
		val newHash3 = iterateHash((newValueHash >>> 16) & 0xFF, newHash2)
		iterateHash(newValueHash >>> 24, newHash3)
	}
	final def iterateHash(key: Int, previousHash: Int): Int =
	{
		val hash1 = previousHash + key
		val hash2 = hash1 + (hash1 << 10)
		hash2 ^ (hash2 >>> 6)
	}
}