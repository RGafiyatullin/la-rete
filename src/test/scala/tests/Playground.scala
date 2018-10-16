package tests

import com.github.rgafiyatullin.la_rete.processors.trie.TrieProcessor
import com.github.rgafiyatullin.la_rete.processors.naive.NaiveProcessor
import com.github.rgafiyatullin.la_rete.{Filter, Processor, Property}
import com.github.rgafiyatullin.xml.common.QName
import com.github.rgafiyatullin.xml.dom.Node
import com.github.rgafiyatullin.xmpp_protocol.XmppConstants
import com.github.rgafiyatullin.xmpp_protocol.stanza_error.XmppStanzaError
import com.github.rgafiyatullin.xmpp_protocol.stanzas.Stanza
import com.github.rgafiyatullin.xmpp_protocol.stanzas.iq.{IQ, IQType}
import com.github.rgafiyatullin.xmpp_protocol.stanzas.message.{Message, MessageType}
import com.github.rgafiyatullin.xmpp_protocol.stanzas.presence.{Presence, PresenceType}

final class Playground extends TestBase {
  val testSize = 20

  def createRules(matches: Matches): Seq[Processor.Rule[Stanza, (Int, Int)]] = {
    import matches.filters

    val stanzaIsRequestIQ = filters.stanza.isIQ and filters.iq.isRequest
    val stanzaIsResponseIQ = filters.stanza.isIQ and filters.iq.isResponse
    val stanzaIsRequestMessage = filters.stanza.isMessage and filters.message.isRequest
    val stanzaIsErrorMessage = filters.stanza.isMessage and filters.message.isError
    val stanzaIsAvailabilityPresence = filters.stanza.isPresence and filters.presence.isAvailability
    val stanzaIsSubscriptionPresence = filters.stanza.isPresence and filters.presence.isSubscription
    val stanzaIsErrorPresence = filters.stanza.isPresence and filters.presence.isError

    def stanzasWithIDandQN(qn: QName, n: Int): Seq[Processor.Rule[Stanza, (Int, Int)]] =
      for (i <- 0 to testSize) yield
        if (i == testSize)
          (stanzaIsRequestIQ and filters.payload(qn)) -> (n, 0)
        else if (i % 2 == 0)
          (stanzaIsRequestIQ and filters.payload(qn) and filters.idLength(i)) -> (n, i)
        else
          (stanzaIsRequestIQ and filters.idLength(i) and filters.payload(qn)) -> (n, i)

    val stanzaIsRequestIQRosterQuery = stanzasWithIDandQN(XmppConstants.names.jabber.iq.roster.query, 8)
    val stanzaIsRequestIQXmppBind = stanzasWithIDandQN(XmppConstants.names.urn.ietf.params.xmlNs.xmppBind.bind, 9)

    stanzaIsRequestIQRosterQuery ++
      stanzaIsRequestIQXmppBind ++
      Seq[Processor.Rule[Stanza, (Int, Int)]](
        stanzaIsRequestIQ -> (1, 0),
        stanzaIsResponseIQ -> (2, 0),
        stanzaIsRequestMessage -> (3, 0),
        stanzaIsErrorMessage -> (4, 0),
        stanzaIsAvailabilityPresence -> (5, 0),
        stanzaIsSubscriptionPresence -> (6, 0),
        stanzaIsErrorPresence -> (7, 0) )

//    stanzaIsRequestIQRosterQuery
  }

  def testData: Seq[(Stanza, (Int, Int))] = {
    type Item = (Stanza, (Int, Int))
    val rosterIQs: Seq[Item] =
      for (i <- 0 until testSize)
        yield IQ.request(Node(XmppConstants.names.jabber.iq.roster.query)).withId("".padTo(i, '_')) -> (8, if (i < testSize) i else 0)
    val bindIQs: Seq[Item] =
      for (i <- 0 until testSize)
        yield IQ.request(Node(XmppConstants.names.urn.ietf.params.xmlNs.xmppBind.bind)).withId("".padTo(i, '_')) -> (9, if (i < testSize) i else 0)

    rosterIQs ++
      bindIQs ++
      Seq[Item](
        IQ.result("1") -> (2, 0),
        IQ.error("2", XmppStanzaError.ServiceUnavailable()) -> (2, 0),

        Message.chat -> (3, 0),
        Message.groupchat -> (3, 0),
        Message.error(XmppStanzaError.FeatureNotImplemented()) -> (4, 0),
        Presence.availability(PresenceType.Available) -> (5, 0),
        Presence.subscription(PresenceType.Subscribe) -> (6, 0),
        Presence.error(XmppStanzaError.ResourceConstraint()) -> (7, 0)
      )
//    rosterIQs
  }

  def runTest(processorFactory: Processor.Factory): Unit = {
    val m = new Matches
    val rules = createRules(m)

    val t0c = System.nanoTime()
    val processor = processorFactory.createProcessor(rules)
    val t1c = System.nanoTime()
    println(s"$processorFactory -> dt[compile]=${(t1c - t0c) / 1000000}ms")

    val t0e = System.nanoTime()
    testData.foreach {
      case (stanza, result) =>
        processor.process(stanza) should contain (result)
    }
    val t1e = System.nanoTime()
    println(s"$processorFactory -> dt[exec]=${(t1e - t0e) / 1000000}ms")
  }




  "matches" should "instantiate" in { new Matches; () }

  it should "work with NaiveProcessor" in runTest(NaiveProcessor)

  it should "work with CheekyProcessor" in runTest(TrieProcessor)

  "constTrue" should "always pass" in {
    val rules = Seq(Filter.constTrue[Any] -> Unit)
    val processor = TrieProcessor.createProcessor(rules)
    (1 to 100) foreach { item =>
      processor.process(item).isDefined should be (true)
    }
  }

  "constFalse" should "always reject" in {
    val rules = Seq(Filter.constFalse[Any] -> Unit)
    val processor = TrieProcessor.createProcessor(rules)
    (1 to 100) foreach { item =>
      processor.process(item).isEmpty should be (true)
    }
  }
}
