package net.frostbridge.xml

import org.scalacheck._

class ContentState(config: ContentConfig, currentDepth: Int)
{
	def down = new ContentState(config, currentDepth + 1)
	def elementsAllowed: Gen[Boolean] =
		for(noElements <- config.onlyLeaves) yield !noElements && currentDepth <= config.maxDepth
}
class ContentConfig(val children: Gen[Int], val onlyLeaves: Gen[Boolean], val attributeCount: Gen[Int], val maxDepth: Int)
{
	def rootState = new ContentState(this, 0)
}
class StringConfig(val contentLength: Gen[Int], val nameLength: Gen[Int],
	val schemeLength: Gen[Int], val sspLength: Gen[Int], val cdata: Gen[Boolean])
class XMLConfig(val content: ContentConfig, val strings: StringConfig, namespaceCountOption: Option[Int])
{
	import java.net.URI
	import javax.xml.stream.events.Namespace
	
	import com.ctc.wstx.stax.WstxEventFactory
	val factory = new WstxEventFactory
	
	private val namespaceG: Gen[Namespace] =
	{
		import strings._
		for(scheme <- GenXMLStrings.alphaLowerString(schemeLength); ssp <- GenXMLStrings.nonEmptyString(sspLength)) yield
			factory.createNamespace(new URI(scheme, ssp, null).toString)
	}
	private val namespaces: Option[Array[Namespace]] =
	{
		for(namespaceCount <- namespaceCountOption) yield
			Array.fromFunction(createNamespace _)(namespaceCount).flatMap(_.toList).toArray
	}
	
	private[this] def createNamespace(i: Int): Option[Namespace] = namespaceG.sample
	
	def namespaces_? : Boolean = namespaceCountOption.isDefined
	
	def namespace: Option[Gen[Namespace]] =
	{
		for(ns <- namespaces) yield
			for(index <- Gen.choose(0, ns.length - 1)) yield
				ns(index)
	}
}
