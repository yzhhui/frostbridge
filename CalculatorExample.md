# Introduction #

This example demonstrates a simple calculator that evaluates simple expressions written in XML.  Numeric literals are wrapped in a `num` tag:
```
 <num>3</num>
```
and functions are written using the function name as the name of the element enclosing the arguments, which can be a number or another function:
```
 <add>
   <num>2</num>
   <sin>
     <num>0.1</num>
   </sin>
 </add>
```

The available functions are:
```
add, subtract, divide, multiply, abs, sqrt, pow, log, max, min, sin, cos, tan, asin, acos, atan, atan2
```

The example assumes you have [Scala](http://www.scala-lang.org) version 2.7.1 or higher installed and that you download everything in the following section to the same directory.

# Download #

  * The Frostbridge [core library](http://frostbridge.googlecode.com/files/frostbridge-0.2.1.jar)
  * The [calculator example](http://frostbridge.googlecode.com/files/frostbridge-0.2.1-tests.jar)
  * The [Woodstox parser](http://woodstox.codehaus.org/3.2.6/wstx-lgpl-3.2.6.jar) (the only runtime dependency for Frostbridge)
  * The calculator example [source code](http://code.google.com/p/frostbridge/source/browse/trunk/src/test/scala/net/frostbridge/example/Calculator.scala) for reference and inline comments
  * An initial [expression document](http://code.google.com/p/frostbridge/source/browse/trunk/src/test/resources/exp.xml)


# Run #

1. Put the jars on your classpath and run the scala interpreter (again, version 2.7.1 or later):
```
scala -cp frostbridge-0.2.1.jar:frostbridge-sitemap-example-0.2.1.jar:wstx-lgpl-3.2.6.jar
```

2. To evaluate a document containing an expression:
```
scala> import net.frostbridge.example._
scala> (new PatternTest(Calculator.exp))("exp.xml")
```