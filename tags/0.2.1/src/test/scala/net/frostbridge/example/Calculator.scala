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
package net.frostbridge.example

import net.frostbridge._
import data._
import Implicits._
import PatternFactory._

/**
* This example demonstrates a simple calculator that evaluates simple
* expressions written in XML.  Numeric literals are wrapped in a 'num' tag:
* &lt;num>3&lt;/num>
* and functions are written using the function name as the name of the element
* enclosing the arguments, which can be a number or another function:
* &lt;add>
*   &lt;num>2&lt;/num>
*   &lt;num>3&lt;/num>
* &lt;/add>
* 
* The available functions are: add, subtract, divide, multiply, abs, sqrt, pow,
* log, max, min, sin, cos, tan, asin, acos, atan, atan2.  Others are easily added.
*
* To run this example, pass Calculator.exp as the argument to the PatternTest
* constructor and follow the instruction on running PatternTest with echoResult=true.
*/
object Calculator
{
	/** The name of an element that wraps a numeric literal. */
	val numberName = "num"
	
	/** Defines a numeric literal to be a text element with name given by 'numberName'
	* and text content of any double value.*/
	val number = textElement(numberName, AnyDouble)
	/** Defines a function to be an element with any name besides 'numberName'
	* and with content determined by the function name.  The content is a list
	* of expressions, with the valid number determined by the function. */
	val function =
		new DynamicElementPattern[Double, Seq[Double]] with UnmarshalOnly[Double]
		{
			/** Define the name class to be any name besides the name of a numeric literal element. */
			val nameClass = AnyName - numberName
			/** Evaluate the function specified by the local part of the name (the namespace is ignored).*/
			def generate(name: QName, g: Seq[Double]) = functionTable(name.localPart).eval(g)
			/** Determine the allowed number of element for the function specified by the local part
			* of the name (the namespace is ignored) and specify the content of the element to be
			* 'exp' repeated minArgs to maxArgs times.  If the function does not exist, the content
			* is NotAllowedPattern. */
			def childrenPattern(actualName: QName) =
				functionTable.get(actualName.localPart).map(g => exp(g.minArgs, g.maxArgs)).getOrElse(NotAllowedPattern)
			def childrenDescription = ""
		}
		
	/** Defines an expression as a function or a numeric literal. */
	val exp: Pattern[Double] = function | number

	/** Creates a function named 'name' with
	*  2 <= 'min' <= argument count <= 'max'
	* that is applied to a list of arguments 'args' using:
	*   args reduceLeft 'reduce' */
	def op(name: String, min: Int, max: UpperBound, reduce: (Double, Double) => Double) =
	{
		require(min >= 2 && max >= min)
		(name, F(name, min, max, seq => seq reduceLeft reduce) )
	}
	import Math._
	/** Table of defined functions that maps a function name to a function definition. */
	val functionTable = Map(
		f("sin", sin _),
		f("cos", cos _),
		f("tan", tan _),
		f("asin", sin _),
		f("acos", cos _),
		f("atan", atan _),
		f("log", log _ ),
		f("sqrt", sqrt (_: Double) ),
		f("pow", pow(_,_) ),
		f("atan2", atan2(_,_) ),
		f("abs", abs(_: Double)),
		op("add", 2, Infinite, _ + _),
		op("subtract", 2, 2, _ - _),
		op("divide", 2, 2, _ / _),
		op("multiply", 2, Infinite, _ * _),
		op("max", 2, Infinite, max(_, _) ),
		op("min", 2, Infinite, min(_, _) )
	)
		
	/** Creates a function defintion for a one argument function*/
	def f(name: String, g: Double => Double) = (name, F(name, 1, 1, seq => g(seq(0)) ))
	/** Creates a function defintion for a two argument function*/
	def f(name: String, g: (Double, Double) => Double) = (name, F(name, 2, 2, seq => g(seq(0), seq(1)) ))
	
	/** Function definition for a function named 'name', that accepts 'minArgs' to 'maxArgs'
	* arguments, and is defined by 'eval'.*/
	case class F(name: String, minArgs: Int, maxArgs: UpperBound, eval: Seq[Double] => Double)
}