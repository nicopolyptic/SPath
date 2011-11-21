package examples

import xspath.XSPathLite
import javax.xml.parsers.{DocumentBuilderFactory}
import javax.xml.xpath.{XPathConstants, XPathFactory}
import org.w3c.dom.{NodeList}
import java.util.Date
import javax.xml.namespace.QName
import xml.Node
import collection.JavaConversions
import management.{MemoryMXBean, MemoryPoolMXBean}


object XpathMarkExamples extends XSPathLite {

  val site = Element("site")
  val closed_auctions = Element("closed_auctions")
  val closed_auction = Element("closed_auction")
  val annotation = Element("annotation")
  val description = Element("description")
  val text = Element("text")
  val bold = Element("bold")
  val emph = Element("emph")
  val keyword = Element("keyword")
  val date = Element("date")
  val profile = Element("profile")
  val gender = Element("gender")
  val age = Element("age")
  val name = Element("name")
  val people = Element("people")
  val person = Element("person")
  val phone = Element("phone")
  val homepage = Element("homepage")
  val address = Element("address")
  val creditcard = Element("creditcard")
  val regions = Element("regions")
  val item = Element("item")
  val samerica = Element("samerica")
  val namerica = Element("namerica")
  val listitem = Element("listitem")
  val open_auction = Element("open_auction")
  val open_auctions = Element("open_auctions")
  val bidder = Element("bidder")
  val income = Attribute("income")
  val interval = Element("interval")
  val increase = Element("increase")
  val current = Element("current")
  val watches = Element("watches")
  val watch = Element("watch")
  val person_id = Attribute("person")
  val open_auction_id = Attribute("open_auction")
  val seller = Element("seller")
  val id = Attribute("id")
  val itemref = Element("itemref")
  val australia = Element("australia")
  val item_id = Attribute("item")
  val city = Element("city")
  val initial = Element("initial")
  val europe = Element("europe")
  val parlist = Element("parlist")
  val personref = Element("personref")
  val catgraph = Element("catgraph")
  val edge = Element("edge")
  val from = Attribute("from")
  val to = Attribute("to")

  val doc = scala.xml.XML.load(this.getClass().getResourceAsStream("/doc.xml"))

  val xpath = xPath
  val dom = domDoc

  cacheDuringEvalution = true

  // A1 Keywords in annotations of closed auctions {child}
  val A1_XPath = "/site/closed_auctions/closed_auction/annotation/description/text/keyword"
  //A2 Keywords in annotations of closed auctions {descendant}
  val A2_XPath = "//closed_auction//keyword"
  //A3 Keywords in annotations of closed auctions {child and descendant}
  val A3_XPath = "/site/closed_auctions/closed_auction//keyword"
  //A4 Closed auctions with an annotation containing a keyword {filter with child}
  val A4_XPath = "/site/closed_auctions/closed_auction[annotation/description/text/keyword]/date"
  //A5 Closed auctions with an annotation containing a keyword {filter with descendant}
  val A5_XPath ="/site/closed_auctions/closed_auction[descendant::keyword]/date"
  //A6 People that have declared both gender and age {conjunction in filters}
  val A6_XPath = "/site/people/person[profile/gender and profile/age]/name"
  //A7 People that have declared either phone or homepage {disjunction in filters}
  val A7_XPath = "/site/people/person[phone or homepage]/name"
  //A8 People that have declared address and either phone or homepage and either credit card or profile {conjunction and disjunction in filters}
  val A8_XPath = "/site/people/person[address and (phone or homepage) and (creditcard or profile)]/name"
  //B1 American items {parent}
  val B1_XPath = "/site/regions/*/item[parent::namerica or parent::samerica]/name"
  //B2 Paragraph items containing a keyword {ancestor}
  val B2_XPath = "//keyword/ancestor::listitem/text/keyword"
  //B3 Bidders except the last one of each open auction {following-sibling}
  val B3_XPath = "/site/open_auctions/open_auction/bidder[following-sibling::bidder]"
  //B4 Bidders except the first one of each open auction {preceding-sibling}
  val B4_XPath = "/site/open_auctions/open_auction/bidder[preceding-sibling::bidder]"
  //B5 Items of the document except the last one {following}
  val B5_XPath = "/site/regions/*/item[following::item]/name"
  //B6 Items of the document except the first one {preceding}
  val B6_XPath = "/site/regions/*/item[preceding::item]/name"
  //B7 People that have declared an income {attribute}
  val B7_XPath = "//person[profile/@income]/name"
  //B8 Open auctions with exactly one bidder {filter with negation}
  val B8_XPath = "/site/open_auctions/open_auction[bidder and not(bidder/preceding-sibling::bidder)]/interval"
  //B9 Open auctions {propositional reasoning: the filter is always true (it is a tautology)}
  val B9_XPath = "/site/open_auctions/open_auction[(not(bidder/following::bidder) or not(bidder/preceding::bidder)) or (bidder/following::bidder and bidder/preceding::bidder)]/interval"
  // B10 No open auction {propositional reasoning: the filter is always false (it is a contraddiction)}
  val B10_XPath = "/site/open_auctions/open_auction[(not(bidder/following::bidder) or not(bidder/preceding::bidder)) and(bidder/following::bidder and bidder/preceding::bidder)]/interval"
  //C1 People of age having an income less than 10000 and living is a city that is not Dallas {comparing the string value of a path expression with an atomic values}
  val C1_XPath = "/site/people/person[profile/age >= 18 and profile/@income < 10000 and address/city != \"Dallas\"]/name"
  // C2 Open auctions having some bidder's increase equal to the current amount {comparing the string value of two path expressions}
  val C2_XPath = "/site/open_auctions/open_auction[bidder/increase = current]/interval"
  // C3 People with an income equal to the current price of some item {join on values}
  val C3_XPath = "/site/people/person[profile/@income = /site/open_auctions/open_auction/current]/name"
  // C4 People that are sellers in an auction that they are watching {join on keys}
  val C4_XPath = "/site/people/person[watches/watch/id(@open_auction)/seller/@person = @id]/name"
  //C5 The person corresponding to a give key {id() function on a static parameter}
  val C5_XPath = "id(\"person0\")/name"
  // C6 Open auctions that someone is watching {id() function on a dynamic parameter. Single chasing}
  val C6_XPath = "/site/people/person/watches/watch/id(@open_auction)/interval"
  // C7 People that are watching an auction that sells an Australian item {id() function on a dynamic parameter. Double chasing}
  val C7_XPath = "/site/people/person[watches/watch/id(@open_auction)/itemref/id(@item)/parent::australian]/name"
  // D1 Open auctions with an odd number of bidders {counting}
  val D1_XPath = "/site/open_auctions/open_auction[(count(bidder) mod 2) = 0]/interval"
  // D2 The number of pieces of prose contained in the document {counting}
  val D2_XPath = "count(//text) + count(//bold) + count(//emph) + count(//keyword)"
  // D3 Open auctions with a total increase greater than 10 times the initial price {summing}
  val D3_XPath = "/site/open_auctions/open_auction[sum(bidder/increase) > 10 * initial]/interval"
  // D4 Open auctions with a total increase different from the difference between the current and initial prices {summing}
  val D4_XPath = "/site/open_auctions/open_auction[sum(bidder/increase) != (current - initial)]/interval"
  // D5 Open auctions with an average increase greater than the double of the initial price {taking the average, i.e. summing and counting}
  val D5_XPath = "/site/open_auctions/open_auction[bidder and (sum(bidder/increase) div count(bidder)) > 2 * initial]/interval"
  //E1 Open auctions whose increase in the median position is contained between the first and the last increase of the auction {accessing elements with a specific position wrt child axis}
  val E1_XPath = "site/open_auctions/open_auction[bidder[1]/number(increase) < bidder[floor((last() + 1) div 2)]/number(increase) and bidder[floor((last() + 1) div 2)]/number(increase) < bidder[last()]/number(increase)]/interval"
  //E2 The last keyword appearing in the description of an European item {accessing elements with a specific position wrt descendant axis}
  val E2_XPath = "/site/regions/europe/item/description/descendant::keyword[last()]"
  //E3 The closest paragraph item containing a keyword {accessing elements with a specific position wrt ancestor axis}
  val E3_XPath = "//keyword/ancestor::listitem[1]/text/keyword"
  //E4 Bidders of an open auction whose increase is between the increase of the previous and next bidders of the auction {accessing elements with a specific position wrt sibling axes}
  val E4_XPath = "site/open_auctions/open_auction/bidder[preceding-sibling::bidder[1]/number(increase) <= number(increase) and number(increase) <= following-sibling::bidder[1]/number(increase)]"
  //E5 Items that have at least 100 items following them in the document and at least 100 items preceding them in the document {accessing elements with a specific position wrt following and preceding axes}
  val E5_XPath = "/site/regions/*/item[preceding::item[100] and following::item[100]]/name"
  //E6 Items whose description contains the name of the item {searching for a string}
  val E6_XPath = "/site/regions/*/item[contains(description, name)]/name"
  //E7 Items whose description contains the word 'passion' followed by the word 'eros' followed by the word 'dangerous' {searching for a string}
  val E7_XPath = "/site/regions/*/item[contains(substring-before(description, \"eros\"), \"passion\") and contains(substring-after(description, \"eros\"), \"dangerous\")]/name"
  //E8 Items with a long description {string processing}
  val E8_XPath = "/site/regions/*/item[string-length(translate(normalize-space(description),\" \",\"\")) > 10000]/name"

  val B3_SPath2 = site\open_auctions\open_auction\bidder?(\(followingSibling, bidder))
  val B4_SPath2 = site\open_auctions\open_auction\bidder?(\(precedingSibling, bidder))

  val A1_SPath = site\closed_auctions\closed_auction\annotation\description\text\keyword
  val A2_SPath = \\(closed_auction)\\keyword
  val A3_SPath = site\closed_auctions\closed_auction\\keyword
  val A4_SPath = site\closed_auctions\closed_auction?(\(annotation)\description\text\keyword)\date
  val A5_SPath = site\closed_auctions\closed_auction?(\\(keyword))\date
  val A6_SPath = site\people\person?(\(profile)\gender)?(\(profile)\age)\name
  val A7_SPath = site\people\person?(\(phone or homepage))\name
  val A8_SPath = site\people\person?(\(address))?(\(phone or homepage))?(\(creditcard or profile))\name
  val B1_SPath = site\regions\*\item?(\(parent, namerica or samerica))\name
  val B2_SPath = \\(keyword)\(ancestor, listitem)\text\keyword
  val B3_SPath = site\open_auctions\open_auction\(\(bidder)$rtrim(1))
  val B4_SPath = site\open_auctions\open_auction\(\(bidder)$ltrim(1))
  val B5_SPath = site\regions\*\item $rtrim(1)\name
  val B6_SPath = site\regions\*\item $ltrim(1)\name
  val B7_SPath = \\(person?(\(profile(income))))\name
  val B8_SPath = site\open_auctions\open_auction(\(bidder)$size(1))\interval
  val C1_SPath = site\people\person?(\(profile)\age >= 18 and ?(\(profile(income < 10000)))
    and ?(\(address)\city <> "Dallas"))\name
  val C2_SPath = site\open_auctions\open_auction?(\(bidder)\increase join \(current))\interval
  val C4_SPath = site\people\person(id on (person_id, \(watches)\watch\$id(open_auction)\seller))\name
  val C5_SPath = \\(id == "person0")
  val C6_SPath = site\people\person\watches\watch\$id(open_auction)\interval
  val C7_SPath = site\people\person?(\(watches)\watch\$id(open_auction)\itemref\$id(item)\(parent, australia))\name
  val D1_SPath = site\open_auctions\open_auction(\(bidder)$context((s:Int) => s % 2 == 0))\interval
  val D2_SPath = \\(text or bold or emph or keyword)
  val E1_SPath = site\open_auctions\open_auction?(
    (((\(bidder)$first)\increase) < (\(bidder)$nth((s:Int) => (s+1)/2)\increase))
      and ((\(bidder)$nth((s:Int) => (s+1)/2)\increase) < ((\(bidder)$last)\increase))
    )\interval

  val E2_SPath = site\regions\europe\item\description\(\(descendant, keyword)$first)
  val E3_SPath = \\(keyword)\(\(ancestor, listitem)$last)\text\keyword
  val E4_SPath = site\open_auctions\open_auction\bidder?(
    \->(leftSibling,bidder)\increase <= \(increase) and \(increase) <= \->(rightSibling,bidder)\increase)
  val E5_SPath = site\regions\*\item $ltrim(100)$rtrim(100)\name
  val F1_SPath = \\(bidder)?(\(increase) <= 10 and \->(rightSibling, bidder and \(increase) > 10))
  val F2_SPath = \\(bidder)?(\(increase) <= 10 and \->(leftSibling, bidder and \(increase) > 10))
  val x = $(\(parlist)\item)
  val F3_SPath = \\(listitem)\\(compose(x, 2))\text\keyword
  val F4axis = $(\(seller)\$id(person)\watches\watch\$id(open_auction))
  val F4_SPath = site\open_auctions\open_auction $range(1,5)\\F4axis\interval
  val F5axis = $(\(watches)\watch\$id(open_auction)\bidder\personref\$id(person))
  val F5_SPath = site\people\person $range(1,5)\\F5axis\name
  val F7axis : axis = n => $(n, ~\(catgraph)\edge(from == to @@ n))
  val F7_SPath = site\catgraph\edge(from == "category0")\\F7axis\$id(to)

  def main(args: Array[String]) {
    index (doc)

    println($(doc, \\(person)).size)
    println($(doc, \\(bidder)).size)
    println($(doc, \\(open_auction)).size)

    execute(A1_SPath, "A1_SPath");
    execute(A2_SPath, "A2_SPath");
    execute(A3_SPath, "A3_SPath");
    execute(A4_SPath, "A4_SPath");
    execute(A5_SPath, "A5_SPath");
    execute(A6_SPath, "A6_SPath");
    execute(A7_SPath, "A7_SPath");
    execute(A8_SPath, "A8_SPath");

    execute(B1_SPath, "B1_SPath");
    execute(B2_SPath, "B2_SPath");
    execute(B3_SPath, "B3_SPath");
    execute(B4_SPath, "B4_SPath");
    execute(B5_SPath, "B5_SPath");
    execute(B6_SPath, "B6_SPath");
    execute(B7_SPath, "B7_SPath");
    execute(B8_SPath, "B8_SPath");

    execute(C1_SPath, "C1_SPath");
    execute(C2_SPath, "C2_SPath");
    execute(C4_SPath, "C4_SPath");
    execute(C5_SPath, "C5_SPath");
    execute(C6_SPath, "C6_SPath");
    execute(C7_SPath, "C7_SPath");

    execute(D1_SPath, "D1_SPath");
    execute(D2_SPath, "D2_SPath");

    execute(E1_SPath, "E1_SPath");
    execute(E2_SPath, "E2_SPath");
    execute(E3_SPath, "E3_SPath");
    execute(E4_SPath, "E4_SPath");
    execute(E5_SPath, "E5_SPath");

    execute(F1_SPath, "F1_SPath");
    execute(F2_SPath, "F2_SPath");
    execute(F3_SPath, "F3_SPath");
    execute(F4_SPath, "F4_SPath");
    execute(F5_SPath, "F5_SPath");
    execute(F7_SPath, "F7_SPath");

    execute(A1_XPath, "A1_XPath");
    execute(A2_XPath, "A2_XPath");
    execute(A3_XPath, "A3_XPath");
    execute(A4_XPath, "A4_XPath");
    execute(A5_XPath, "A5_XPath");
    execute(A6_XPath, "A6_XPath");
    execute(A7_XPath, "A7_XPath");
    execute(A8_XPath, "A8_XPath");

    execute(B1_XPath, "B1_XPath");
    execute(B2_XPath, "B2_XPath");
    execute(B3_XPath, "B3_XPath");
    execute(B4_XPath, "B4_XPath");
    execute(B5_XPath, "B5_XPath");
    execute(B6_XPath, "B6_XPath");
    execute(B7_XPath, "B7_XPath");
    execute(B8_XPath, "B8_XPath");

    execute(C1_XPath, "C1_XPath");
    execute(C2_XPath, "C2_XPath");
    execute(C4_XPath, "C4_XPath");
    execute(C5_XPath, "C5_XPath");
    execute(C6_XPath, "C6_XPath");
    execute(C7_XPath, "C7_XPath");

    execute(D1_XPath, "D1_XPath");
    execute(D2_XPath, "D2_XPath");

    execute(E1_XPath, "E1_XPath");
    execute(E2_XPath, "E2_XPath");
    execute(E3_XPath, "E3_XPath");
    execute(E4_XPath, "E4_XPath");
    execute(E5_XPath, "E5_XPath");

    println("\tSPath\t\tXPath time (ms)")
    for (q <- queries) {
      print(q); print("\t")
      print(spathTimeResults.get(q).get)
      print("\t\t")
      println(xpathTimeResults.get(q) match {case Some(t) => t case None => "_"})
    }
    println("\n")

    println("\tSPath\t\tXPath size")
    for (q <- queries) {
      print(q); print("\t")
      print(spathSizeResults.get(q).get)
      print("\t\t")
      println(xpathSizeResults.get(q) match {case Some(n) => n case None => "_"})
    }

    println("")
    println("\tSPath time")
    for (q <- queries) {
      print(q); print("\t")
    }
    println("")
    for (q <- queries) {
      print(spathTimeResults.get(q).get);print("\t")
    }
    println("")


    removeIndex(doc)
    printMemoryUsage

  }

  def break = {
    println("")
    println("-=-=-=-=-=-=-=-=-=-=")
    println("")
  }


  var spathTimeResults = Map[String, Double]()
  var spathSizeResults = Map[String, Int]()

  var xpathTimeResults = Map[String, Double]()
  var xpathSizeResults = Map[String, Int]()

  var queries = scala.collection.mutable.ListBuffer[String]()

  def execute(qs : Array[Query], setname : String) {
    var i = 1
    for (query <- qs) {
      execute(query, setname + i +  " SPath. ")
      i += 1
    }
  }

  def execute (q : Query, msg : String) {
    println(msg)
    var before = new Date
    val result = $(doc, q)
    println("\t size = " + result.size)
   // println("\t size = " + (result map (n => n.label)))
//     println("\t size = " + (result map id))
     //println("\t size = " + result)
    val time = new Date(new Date().getTime - before.getTime).getTime/ 1000D
    println("\t time =  " + time)
    val query = msg.split("_")(0)
    spathSizeResults += query -> result.size
    spathTimeResults += query -> time
    queries += query
  }

  def execute(query : String, msg : String) :Unit = execute(query, msg, XPathConstants.NODESET)

  def execute(query : String, msg : String, rtype : QName) = {
    println(msg)
    var result :Object = null
    val before = new Date()
    try {
      val expr = xpath.compile(query);
      result = expr.evaluate(dom, rtype);
    } catch {case  e:Exception => }
    val time = new Date(new Date().getTime - before.getTime).getTime/ 1000D
    val key = msg.split("_")(0)

    result match {
      case l : NodeList => println("\tsize = " + l.getLength); xpathSizeResults += key -> l.getLength;xpathTimeResults += key -> time
      case n : Number => println("\t number result = " + n); xpathSizeResults += key -> n.intValue();xpathTimeResults += key -> time
      case _ =>
    }
    println("\ttime = " + time)

  }


  def execute (qs : Array[String], setname : String) {
    var i = 1
    for (query <- qs) {
      execute(query, setname + i +  " XPath. ")
      i += 1
    }
  }

  def xPath = {
    val factory = XPathFactory.newInstance()
    factory.newXPath()
  }

  def domDoc = {
    val docfactory = DocumentBuilderFactory.newInstance();
    val builder = docfactory.newDocumentBuilder();
    builder.parse("D:\\work\\xmark\\doc.xml");
  }

  def printMemoryUsage = {

    val beans: java.util.List[MemoryPoolMXBean] = java.lang.management.ManagementFactory.getMemoryPoolMXBeans();
    for (bean <- JavaConversions.asScalaIterable(beans)) {
      println(bean.getName() + " " + bean.getPeakUsage().getUsed().toDouble / 1048576d)
    }

    val bean: MemoryMXBean = java.lang.management.ManagementFactory.getMemoryMXBean()
    println("heap " + bean.getHeapMemoryUsage().getUsed().toDouble / 1048576d)
  }
}