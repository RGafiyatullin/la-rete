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

  final class Matches {
    object properties {
      sealed trait S
      object S {
        case object I extends S
        case object P extends S
        case object M extends S
      }

      object stanza extends Property[Stanza, S] {
        override def toString: String = "s"

        override def apply(obj: Stanza): S =
          obj match {
            case _: IQ => S.I
            case _: Presence => S.P
            case _: Message => S.M
          }
        override def isDefinedAt(obj: Any): Boolean = obj.isInstanceOf[Stanza]
      }

      object iq extends Property[IQ, IQType] {
        override def toString: String = "i"
        override def apply(obj: IQ): IQType = obj.iqType
        override def isDefinedAt(obj: Any): Boolean = obj.isInstanceOf[IQ]
      }

      object message extends Property[Message, MessageType] {
        override def toString: String = "m"
        override def apply(obj: Message): MessageType = obj.messageType
        override def isDefinedAt(obj: Any): Boolean = obj.isInstanceOf[Message]
      }

      object presence extends Property[Presence, PresenceType] {
        override def toString: String = "p"
        override def apply(obj: Presence): PresenceType = obj.presenceType
        override def isDefinedAt(obj: Any): Boolean = obj.isInstanceOf[Presence]
      }

      object paylaod extends Property[IQ.Request, QName] {
        override def toString: String = "q"
        override def apply(obj: IQ.Request): QName = obj.body.qName
        override def isDefinedAt(obj: Any): Boolean = obj.isInstanceOf[Stanza.HasBody.Untyped]
      }
      object idLength extends Property[IQ.Request, Int] {
        override def toString: String = "idl"
        override def apply(obj: IQ.Request): Int = obj.id.length
        override def isDefinedAt(obj: Any): Boolean = obj.isInstanceOf[Stanza.HasID.Untyped]
      }
    }

    object filters {
      object stanza {
        val isIQ: Filter[Stanza, IQ] =
          Filter.property(properties.stanza, properties.S.I).as[IQ]
        val isMessage: Filter[Stanza, Message] =
          Filter.property(properties.stanza, properties.S.M).as[Message]
        val isPresence: Filter[Stanza, Presence] =
          Filter.property(properties.stanza, properties.S.P).as[Presence]
      }
      object iq {
        val isGet: Filter[IQ, IQ.Request] =
          Filter.property(properties.iq, IQType.Get).as[IQ.Request]
        val isSet: Filter[IQ, IQ.Request] =
          Filter.property(properties.iq, IQType.Set).as[IQ.Request]
        val isError: Filter[IQ, IQ.Error] =
          Filter.property(properties.iq, IQType.Error).as[IQ.Error]
        val isResult: Filter[IQ, IQ.Result] =
          Filter.property(properties.iq, IQType.Result).as[IQ.Result]

        val isRequest: Filter[IQ, IQ.Request] = isGet or isSet
        val isResponse: Filter[IQ, IQ.Response] = isError or isResult
      }
      object message {
        val isNormal: Filter[Message, Message.Request] =
          Filter.property(properties.message, MessageType.Normal).as[Message.Request]
        val isChat: Filter[Message, Message.Request] =
          Filter.property(properties.message, MessageType.Chat).as[Message.Request]
        val isGroupchat: Filter[Message, Message.Request] =
          Filter.property(properties.message, MessageType.Groupchat).as[Message.Request]
        val isHeadline: Filter[Message, Message.Request] =
          Filter.property(properties.message, MessageType.Headline).as[Message.Request]
        val isError: Filter[Message, Message.Error] =
          Filter.property(properties.message, MessageType.Error).as[Message.Error]

        val isRequest: Filter[Message, Message.Request] =
          isNormal or isChat or isGroupchat or isHeadline
      }
      object presence {
        val isAvailable: Filter[Presence, Presence.availability.P] =
          Filter.property(properties.presence, PresenceType.Available).as[Presence.availability.P]

        val isUnavailable: Filter[Presence, Presence.availability.P] =
          Filter.property(properties.presence, PresenceType.Unavailable).as[Presence.availability.P]

        val isAvailability: Filter[Presence, Presence.availability.P] =
          isAvailable or isUnavailable

        val isSubscribe: Filter[Presence, Presence.subscription.P] =
          Filter.property(properties.presence, PresenceType.Subscribe).as[Presence.subscription.P]

        val isSubscribed: Filter[Presence, Presence.subscription.P] =
          Filter.property(properties.presence, PresenceType.Subscribed).as[Presence.subscription.P]

        val isUnsubscribe: Filter[Presence, Presence.subscription.P] =
          Filter.property(properties.presence, PresenceType.Unsubscribe).as[Presence.subscription.P]

        val isUnsubscribed: Filter[Presence, Presence.subscription.P] =
          Filter.property(properties.presence, PresenceType.Unsubscribed).as[Presence.subscription.P]

        val isSubscription: Filter[Presence, Presence.subscription.P] =
          isSubscribe or isUnsubscribe or isSubscribed or isUnsubscribed

        val isError: Filter[Presence, Presence.Error] =
          Filter.property(properties.presence, PresenceType.Error).as[Presence.Error]
      }
      def payload(qn: QName): Filter[IQ.Request, IQ.Request] =
        Filter.property(properties.paylaod, qn)
      def idLength(length: Int): Filter[IQ.Request, IQ.Request] =
        Filter.property(properties.idLength, length)
    }
  }

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
}
