# Introduction #

This example is intended to provide an initial presentation of Frostbridge.  It involves unmarshalling (converting XML to an object) and marshalling (converting an object to XML) a sitemap XML document (see [Google Webmaster Tools](https://www.google.com/webmasters/tools/docs/en/protocol.html)).

The example code first constructs the pattern, which is the template for binding.  It then uses the pattern to unmarshal a sitemap XML document to a `SiteMap` object (defined in the example code).  The example then uses the pattern to marshal the object back to XML.

The example assumes you have [Scala](http://www.scala-lang.org) version 2.7.1 or higher installed and that you download everything in the following section to the same directory.



# Download #

  * The Frostbridge [core library](http://frostbridge.googlecode.com/files/frostbridge-0.2.1.jar)
  * The [sitemap example](http://frostbridge.googlecode.com/files/frostbridge-0.2.1-tests.jar)
  * The [Woodstox parser](http://woodstox.codehaus.org/3.2.6/wstx-lgpl-3.2.6.jar) (the only runtime dependency for Frostbridge)
  * The sitemap example [source code](http://code.google.com/p/frostbridge/source/browse/trunk/src/test/scala/net/frostbridge/example/SiteMap.scala) for reference and inline comments
  * Some sitemap XML documents to process.  For example:
    * A large (~4 MB) sitemap is available at http://www.google.com/sitemap.xml
    * A smaller sitemap is the [Sample XML Sitemap](https://www.google.com/webmasters/tools/docs/en/protocol.html#sitemapXMLExample) on the Google Webmaster Tools page.
    * A sitemap conforming to sitemap version 0.84 is http://www.usa.gov/sitemap.xml

# Run #

1. Put the jars on your classpath and run the scala interpreter (again, version 2.7.1 or later):
```
scala -cp frostbridge-0.2.1.jar:frostbridge-sitemap-example-0.2.1.jar:wstx-lgpl-3.2.6.jar
```


2. We'll need the following imports:
```
scala> import net.frostbridge._
scala> import example.SiteMapExample
scala> import SiteMapExample._
scala> import java.io.{File, OutputStreamWriter}
```


3. This basic test will unmarshal the document specified in the first argument (here the example assumes there is a document called "sitemap.xml" in the current directory), print the generated data object to standard output, marshal it back to XML, and print the XML (a smaller sitemap is probably more useful here).  See the example source code or the bottom of this page for the data object class definitions.
```
scala> SiteMapExample("sitemap.xml", true)
```
The pattern in the example is setup to only match version 0.9 sitemap documents, so if you try an earlier version (like the version 0.84 document mentioned in the download section), you can see basic error handling.


4. You can time the process without printing the results by setting the second argument to false (the Google sitemap is good for this).
```
scala> SiteMapExample("sitemap.xml", false)
```


5. The following shows how to directly initiate unmarshalling:
```
scala> val e = Unmarshaller.unmarshalOrError(siteMapPattern, in.StAXStream(new File("sitemap.xml")))
```
`siteMapPattern` is defined in the example code (see the example source code or the bottom of this page) and is the main template for binding.

`in.StaxStream.apply` converts the given source to a stream of XML nodes required for unmarshalling. It is overloaded to accept a `URL`, `InputStream`, or `Reader`.

The result is `Left[String]` if there was an error (the content is the error message) or `Right[SiteMap]` if it was successful.  (Note that it is possible to have finer control over error handling if desired; this just provides a reasonable error message.)


6. The following shows how to directly initiate marshalling to standard output:
```
scala> val x = Marshaller(e.right.get, siteMapPattern, new OutputStreamWriter(System.out))
```
For no output, use `StringWriter` instead.  The result type is `Option[MarshalException]`, where None indicates success.

7. The real work is defining the pattern and the data classes.  The pattern and data types are listed here for immediate reference, but see the example source code for explanations in the comments.

The data types:
```
case class SiteMap(locations: List[Location])
abstract class Location
{
    val location: URL
    val lastModified: Option[XMLGregorianCalendar]
    val changeFrequency: Option[Frequency#Value]
    val priority: Option[Double]
	
    override def toString = "Location {url=" + location + ", lastModified=" +
        lastModified + ", frequency=" + changeFrequency + ", priority=" + priority + "}"
}
object Frequency extends Frequency
class Frequency extends Enumeration("always", "hourly", "daily", "weekly", "monthly", "yearly", "never")
{
    val Always, Hourly, Daily, Weekly, Monthly, Yearly, Never = Value
}
```

The pattern:
```
val ns = "http://www.sitemaps.org/schemas/sitemap/0.9"
val siteMapPattern: Pattern[SiteMap] =
{
    val location = textElement(ns :: "loc", AnyURL)
    val lastModified = textElement(ns :: "lastmod", AnyDateTime)
    val changeFrequency = textElement[Frequency#Value](ns :: "changefreq", new EnumerationByName(Frequency, "frequency"))
    val priority = textElement(ns :: "priority", new RangedDouble(0.0 to 1.0))

    val urlPattern = location :+: (lastModified?) :+: (changeFrequency?) :+: (priority?)
    val url =
        new element[Location, urlPattern.GeneratedType](ns :: "url", urlPattern)
        {
            def generate(u: urlPattern.GeneratedType) =
                new Location { val location :+: lastModified :+: changeFrequency :+: priority = u }
            def marshalTranslate(l: Location) = Some(l.location :+: l.lastModified :+: l.changeFrequency :+: l.priority)
        }
    new element[SiteMap, Seq[Location]](ns :: "urlset", url+)
    {
        def generate(urls: Seq[Location]) = SiteMap(urls)
        def marshalTranslate(map: SiteMap) = Some(map.locations)
    }
}
```