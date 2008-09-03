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

/** This example demonstrates basic usage of the library.  It shows the three main steps to
* using the library:
* 1) Define the data types that will represent the XML document.
* 2) Define the pattern that represents the expected structure of the XML.  The pattern also defines
*      how to convert between the XML and Scala objects.
* 3) Initiate marshalling or unmarshalling given an XML document or an object.
*
* To run the example from a checked out project using Maven, type
*   mvn scala:console
* in the frostbridge/examples/sitemap directory.
* scala> import net.frostbridge.example.SiteMapExample
* scala> SiteMapExample("data/sitemap.xml")
* scala> SiteMapExample("data/sitemap.xml", false) // to only time the processing
*
* For an idea of performance, processing http://www.google.com/sitemap.xml (downloaded to the
* file system: ~4 MB document with ~35,000 url elements) takes about:
*  Unmarshal: 2 seconds
*  Marshal: 0.7 seconds
* on a Core Duo (T2300, 1.66GHz), 1 GB RAM, Linux kernel 2.6.25, Scala 2.7.1, Java 1.6
*/

// The main library package is net.frostbridge
import net.frostbridge._
import Implicits._

import java.io.{File, OutputStreamWriter, StringWriter}
import java.net.URL
// XMLGregorianCalendar corresponds to the W3C dateTime data type in XML Schema.
import javax.xml.datatype.XMLGregorianCalendar

object SiteMapExample
{
	// Here are the entry methods.  The main operation is constructing an
	// XMLStream from the provided source.  An XMLStream provides a stream of
	// XML nodes from an input source.  StAXStream is the object used by a
	// client to construct an XMLStream from a File, URL, InputStream, or Reader.
	def apply(filename: String): Unit = apply(filename, true)
	def apply(filename: String, echoResult: Boolean): Unit =
	{
		val file = new File(filename)
		if(file.exists)
			test(in.StAXStream(file), echoResult)
		else
			println("File does not exist.")
	}
	def apply(url: URL): Unit = apply(url, true)
	def apply(url: URL, echoResult: Boolean): Unit = test(in.StAXStream(url), echoResult)
	
	// This method umarshals the XML in the given file to a SiteMap object,
	// prints its toString representation, and then marshals it to
	// standard output as XML.
	def test(source: in.XMLStream, echoResult: Boolean)
	{
		val unmarshalStartTime = System.currentTimeMillis
		// The Matcher object is the simplest way to unmarshal XML to an object.  (More control
		// is possible by directly creating a Matcher instance and passing a custom MatchHandler.)
		// The matchOrError method returns Either[String, Generated], where Left contains an
		// error message or Right contains the unmarshalled object of the appropriate type.  Here,
		// that type is SiteMap.
		Unmarshaller.unmarshalOrError(siteMapPattern, source) match
		{
			case Right(parsed) =>
			{
				val unmarshalEndTime = System.currentTimeMillis
				val unmarshalTime = (unmarshalEndTime - unmarshalStartTime)/1000.0
				println("\nSuccessfully unmarshalled sitemap document with " + parsed.locations.length + " URLs in " + unmarshalTime + " s")
				if(echoResult)
				{
					println()
					println(parsed.toString)
					println('\n')
				}
				
				//To marshal an object to XML, use the Marshaller object.  The return value is
				// Option[MarshalException[Generated]].  None represents no error.  See the
				// MarshalException documentation for more information on using this error.
				val marshalStartTime = System.currentTimeMillis
				val writer = if(echoResult) new OutputStreamWriter(System.out) else new StringWriter
				Marshaller(parsed, siteMapPattern, writer) match
				{
					case None =>
					{
						val marshalEndTime = System.currentTimeMillis
						val marshalTime = (marshalEndTime - marshalStartTime)/1000.0
						println("\n\nMarshalled successfully in " + marshalTime +" s.")
					}
					case Some(error) => println(error)
				}
			}
			case Left(e) => println(e.toString)
		}
	}
// =========================== Define the pattern ===========================
	// The namespace for the sitemap protocol.
	val ns = "http://www.sitemaps.org/schemas/sitemap/0.9"
		
	// This constructs the pattern that defines the XML <-> object binding.
	// See https://www.google.com/webmasters/tools/docs/en/protocol.html
	// for a description of a sitemap XML document's structure.
	val siteMapPattern: Pattern[SiteMap] =
	{
		// The net.frostbridge.data package provides data types.
		import data._
		// PatternFactory contains methods for creating patterns
		import PatternFactory._
		
		// Here we define the text element patterns.  The TextElement constructor accepts a NameClass instance
		//   and a data.ValueParser instance. The NameClass instance defines the element names that the pattern
		//   will match.  The ValueParser instance defines the text content that will be matched and how to
		//   convert the content from a String to an object.
		
		// Predefined defines an implicit to convert a local name string to a QName.  QName defines :: to
		//   prepend a namespace URI.  Predefined provides another implicit that converts the QName to a
		//   NameClass that matches names exactly the same as that QName.  See the NameClass subclasses
		//   for other possibilities.
		val location = textElement(ns :: "loc", AnyURL)
		val lastModified = textElement(ns :: "lastmod", AnyDateTime)
		// Enumerations are slightly tricky because of certain difficulties with inner types (see also the
		//   enumeration definition at the bottom of this example).  Here we have to explicitly define the
		//   type that the TextElement corresponds to.
		val changeFrequency = textElement[Frequency#Value](ns :: "changefreq", new EnumerationByName(Frequency, "frequency"))
		// Predefined defines an implicit to convert a double (or integer) to a Ranged instance, which defines the 'to' and 'until'
		// methods similarly to scala.Range and can be used to restrict the allowed values of a number.
		val priority = textElement(ns :: "priority", new RangedDouble(0.0 to 1.0))
		
		// Here we declare the main pattern, which defines the content of a 'url' element.
		//   :+: creates an ordered sequence. (+++ creates an unordered sequence and | creates a set of options.
		//   These are not used here.)
		val urlPattern = location :+: (lastModified?) :+: (changeFrequency?) :+: (priority?)
		
		// Here we tie everything together to declare a 'url ' element
		val url = element(urlPattern,
			new NamedElementOp[Location, urlPattern.GeneratedType]
			{
				val name = ns :: "url"
				// Here we transform the object generated by the above pattern into a Location object.
				// (Type inference is very important to the usability of the library. Explicitly writing urlPattern.GeneratedType gives:
				// 	urlPattern.GeneratedType = (Location, (Option[XMLGregorianCalendar], (Option[Frequency#Value], Option[Double]))) )
				def generate(u: urlPattern.GeneratedType) =
				{
				// Extractor methods (defined on the object :+:) are also important to the usability of the library.  Combined with
				// type inference, the following extracts the values matched by urlPattern and constructs a Location object.  The
				// whole process is statically typed.
					new Location { val location :+: lastModified :+: changeFrequency :+: priority = u }
				}
				// Here we do the reverse transformation for the serialization process.  Predefined defines an implicit
				//   to convert a value to an intermediate type (Anchor) that defines :+: and +++ for a symmetric way
				//   of constructing the nested pairs expected by the ordered and unordered sequence patterns.
				def marshalTranslate(l: Location) = Some(l.location :+: l.lastModified :+: l.changeFrequency :+: l.priority)
			})
		// Here we define the root element as one or more 'url' elements using the postfix + operator.
		//  + is defined on Pattern, as are ? (to define an optional pattern) and * (to match a pattern zero or more times).
		element(url+, 
			new NamedElementOp[SiteMap, Seq[Location]]
			{
				val name = ns :: "urlset"
				def generate(urls: Seq[Location]) = SiteMap(urls)
				def marshalTranslate(map: SiteMap) = Some(map.locations)
			})
		
		// Here is a summary of creating a sequence pattern for use as the content of an element.
		// We use the following existing definitions:
		//   trait ABC { val a: A;  val b: B;  val c: C }
		//   val ap: Pattern[A];  val bp: Pattern[B];  val cp: Pattern[C]
		//   val name: NameClass
		//
		// 1) We construct an unordered sequence pattern using the +++ operator defined on Pattern:
		//      val p = ap +++ bp +++ cp
		//    This creates a pattern that produces data of type ((A, B), C)
		// 2) We define a mapping from this type to our data type using the +++ extractor object.
		//      val unmarshal = (d: p.GeneratedType) => new ABC { val a +++ b +++ c = d }
		// 3) We define a reverse mapping from our data type to the pattern's generated type
		//     using the implicit to the Anchor helper type and the +++ operator defined on Anchor:
		//      val marshal = (abc: ABC) => Some(abc.a +++ abc.b +++ abc.c)
		// 4) The pattern and the mappings are used as arguments to create an element pattern:
		//      element(name, p, unmarshal, marshal)
	}
}

// ====================== Define the data types =============================

// The data types are defined by the user, not the library.  Additionally,
//   the user defines how to construct the data objects in the patterns, so
//   the library does not use any reflection or bytecode manipulation to
//   build the data objects.
case class SiteMap(locations: Seq[Location])
abstract class Location
{
	val location: URL
	val lastModified: Option[XMLGregorianCalendar]
	val changeFrequency: Option[Frequency#Value]
	val priority: Option[Double]
	
	override def toString = "Location {url=" + location + ", lastModified=" + lastModified + ", frequency=" + changeFrequency +
		", priority=" + priority + "}"
}
// The following separate definition of the Enumeration as a class and then having the
//  object inherit the class is required due to difficulties representing inner types
//  (see http://www.nabble.com/-scala--Wrapping-and-referring-to-dependent-types-(in-Enumerations)-tp15921758p15921758.html).
object Frequency extends Frequency
class Frequency extends Enumeration("always", "hourly", "daily", "weekly", "monthly", "yearly", "never")
{
	val Always, Hourly, Daily, Weekly, Monthly, Yearly, Never = Value
}