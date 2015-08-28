# Reference Part I: QName, NameClass, and ValueParser #

## Qualified Name ##
A qualified name, as defined in [XML Namespaces](http://www.w3.org/TR/REC-xml-names/#ns-qualnames), is represented by the `QName` class.  There are two ways to create a `QName`:
  * Directly:
```
 val ns = "http://example.com/ns"
 val localPart = "tag"
 QName(ns, localPart)
```
  * Implicitly:
```
 import Implicits._
 val ns = "http://example.com/ns"
 val localPart = "tag"
 ns :: localPart
```
> This implicitly converts localPart to `QName("", localPart)`.  The right-associative operator `::`, defined on `QName`, creates a new `QName` with the local part of the original and namespace URI `ns` (the argument to `::`).  The operator `::` is only valid on a `QName` without a namespace URI specified.

## Name Classes ##

A `NameClass` defines a valid set of qualified names and is used by attribute and element patterns to match attributes and elements by name.  The various implementations are described below.

  * `AnyName`, as its name implies, matches any instance of `QName` and thus describes the set of names that contains every instance of `QName`.
  * `Name(q: QName)` contains only the qualified name `q`.
  * `NameChoice(q1: QName, q2: QName)` or the equivalent `q1 | q2` contains every instance of `QName` that is contained in `q1` or `q2` or both.
  * `NsName(ns: String)` contains every instance of `QName` with namespace URI exactly equal (see [Comparing URI References](http://www.w3.org/TR/REC-xml-names/#NSNameComparison)) to namespace URI `ns`.
  * `AnyNameExcept(n: NameClass)` or the equivalent `AnyName - n` contains every `QName` that is not contained in the name class `n`.  `n` may not be `AnyName`.
  * `NsNameException(ns: String, except: NameClass)` or the equivalent `NsName(ns) - except` contains every `QName` with namespace URI exactly equal (see [Comparing URI References](http://www.w3.org/TR/REC-xml-names/#NSNameComparison)) to namespace URI `ns` and that is not contained in `except`. `except` cannot be AnyName and can only consist of `Name` and `NameChoice` instances.

## Value Parsers ##

### Description ###
`ValueParser[T]` handles converting between a data type T and its textual representation.  It is used, for example, to handle text content and attribute values.

### Usage ###
Instantiate any of the predefined implementations in the `net.frostbridge.data` package or subclass `BasicParser` (or `ValueParser`).  `BasicParser` implements `ValueParser` in a way that separates validation from parsing, so that validation can be done on both parsing and serializing.

To subclass `BasicParser[T]`, implement the following four methods:
  * `parse(s: String): Option[T]`
> This should convert a `String` into an instance of `T` wrapped in `Some` or return `None` if the string cannot be parsed.  Any thrown `Exception` will be caught by `BasicParser` and turned into `None` (so code like `s.toInt` does not need to be wrapped in a try/catch).  Validation should be handled in `isAllowed`.
  * `stringify(t: T): Option[String]`
> This should convert an instance of `T` into a string representation wrapped in `Some` or return None if the conversion cannot be done.  Any thrown `Exception` will be caught by `BasicParser` and turned into `None` (so code like `s.toInt` does not need to be wrapped in a try/catch).  Validation should be handled in `isAllowed`.
  * `isAllowed(t: T): Boolean`
> This returns true if and only if the object `t` is valid.  `BasicParser` checks this method after `parse` and before `stringify`.
  * `dataDescription: String`
> This provides a description of the expected data type.  It is used when tracing a pattern and in error messages.

### Example ###

```
abstract class IntegerValue extends BasicParser[Int]
{
	def parse(value: String) = Some(value.toInt)
	def stringify(value: Int) = Some(value.toString)
}
object PositiveInteger extends IntegerValue
{
	def isAllowed(value: Int) = value > 0
	def dataDescription = "positive integer"
}
object NegativeInteger extends IntegerValue
{
	def isAllowed(value: Int) = value < 0
	def dataDescription = "negative integer"
}
```