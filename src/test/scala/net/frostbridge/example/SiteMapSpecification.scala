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

import out._
case class TestSiteMap(xml: Seq[Node], expectedValue: Option[SiteMap])
case class TestLocation(xml: Node, expectedValue: Option[Location])

import org.scalacheck.{Arbitrary, Gen, Properties}
object SiteMapSpecification extends Properties("Site Map") with NotNull
{
	/*import java.util.{Date, GregorianCalendar}
	private val dataFactory = javax.xml.datatype.DatatypeFactory.newInstance
	
	val arbPriority = Arbitrary(Gen.frequency(
		(10000,Gen.choose(0.0, 1.0)),
		(2,Gen.choose(Math.MIN_DOUBLE, -Math.EPS_DOUBLE)),
		(2,Gen.choose(1.0+Math.EPS_DOUBLE, Math.MAX_DOUBLE)),
		(1,Gen.value(Math.NaN_DOUBLE)),
		(1,Gen.value(Math.POSITIVE_INFINITY)),
		(1,Gen.value(Math.NEGATIVE_INFINITY))
	))
	val arbFrequency = Arbitrary(Gen.elements(Frequency.elements))
	private def mapDate(d: Date) =
	{
		val cal = new GregorianCalendar
		cal.setTime(d)
		dataFactory.newXMLGregorianCalendar(cal)
	}
	def genLocation: Gen[TestLocation] =
		for(pri <- arbOption(arbPriority);
			modified <- arbOption(Arbitrary.arbDate);
			frequency <- arbOption(arbFrequency)) yield
		{
			val l = new Location
			{
				val location: URL
				val lastModified = modified.map(mapDate)
				val changeFrequency = frequency
				val priority = pri
			}
			val xml = new StringBuilder("<location>")
			if(url.isDefined)
				xml + "<url>" + url.get + "</url>"
			if(pri.isDefined)
				xml + "<priority>" + pri.get + "</priority>"
			xml.append("</location>")
			if(
				"<location>" + if(pri.isDefined
			TestLocation(
		}
			
	def genExpressions(depth: Int, length: Int, params: TreeParameters): Gen[List[TestExpression]] =
		Gen.containerOfN[List, TestExpression](length, genExpression(depth, params))
	
	implicit val arbExpression = Arbitrary(Gen.frequency(
			(5, genExpression(0, TreeParameters(FixedAverage(1.5), MaximumNodes))),
			(1, genExpression(0, TreeParameters(FixedAverage(10.0), MaximumNodes))),
			(5, genExpression(0, TreeParameters(FunctionMin, MaximumNodes))),
			(5, genExpression(0, TreeParameters(FunctionMax, MaximumNodes)))
		))

	def testExpression(sitemap: TestSiteMap): Boolean =
	{
		val expected = sitemap.expectedValue
		TestPattern(SiteMapExample.siteMapPattern, sitemap.xml, expected.isEmpty, false,
			(v: Double) => expected.isDefined && expected.get == v,
			(s: String) => expected.isDefined && verifyMarshal(s, expected.get))
	}
	def verifyMarshal(s: String, map: SiteMap): Boolean =
	{
		val source = in.StAXStream(new java.io.StringReader(s))
		Unmarshaller.unmarshalOrError(SiteMapExample.siteMapPattern, source) match
		{
			case Right(parsed) => parsed == map
			case Left(errorMessage) => false
		}
	}*/
}