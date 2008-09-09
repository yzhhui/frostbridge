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
class StringConfig(val contentLength: Gen[Int], val nameLength: Gen[Int], val cdata: Gen[Boolean])
class XMLConfig(val content: ContentConfig, val strings: StringConfig, val namespaces: NamespaceGenerator)
{
	import com.ctc.wstx.stax.WstxEventFactory
	val factory = new WstxEventFactory
}
