package tests

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

      def paylaod[S <: Stanza.HasBody.Untyped]: Property[S, QName] =
        new Property[S, QName] {
          override def toString: String = "q"
          override def apply(obj: S): QName = obj.body.qName
          override def isDefinedAt(obj: Any): Boolean = obj.isInstanceOf[Stanza.HasBody.Untyped]
        }
      def idLength[S <: Stanza.HasID.Untyped]: Property[S, Int] =
        new Property[S, Int] {
          override def toString: String = "idl"
          override def apply(obj: S): Int = obj.id.length
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
      def payload[S <: Stanza.HasBody.Untyped](qn: QName): Filter[S, S] =
        Filter.property(properties.paylaod[S], qn)
      def idLength[S <: Stanza.HasID.Untyped](length: Int): Filter[S, S] =
        Filter.property(properties.idLength[S], length)
    }
  }

  "matches" should "instantiate" in { new Matches }

  it should "combine #1" in {
    val m = new Matches
    import m.filters

    val stanzaIsRequestIQ = filters.stanza.isIQ and filters.iq.isRequest
    val stanzaIsResponseIQ = filters.stanza.isIQ and filters.iq.isResponse
    val stanzaIsRequestMessage = filters.stanza.isMessage and filters.message.isRequest
    val stanzaIsErrorMessage = filters.stanza.isMessage and filters.message.isError
    val stanzaIsAvailabilityPresence = filters.stanza.isPresence and filters.presence.isAvailability
    val stanzaIsSubscriptionPresence = filters.stanza.isPresence and filters.presence.isSubscription
    val stanzaIsErrorPresence = filters.stanza.isPresence and filters.presence.isError

    val max = 5
    def stanzasWithIDandQN(qn: QName): Seq[Processor.Rule[Stanza, Any]] =
      for (i <- 0 to max) yield
        if (i == max)
          (stanzaIsRequestIQ and filters.payload[IQ.Request](qn)) -> (qn.hashCode(), 0)
        else if (i % 2 == 0)
          (stanzaIsRequestIQ and filters.payload[IQ.Request](qn) and filters.idLength[IQ.Request](i)) -> (qn.hashCode(), i)
        else
          (stanzaIsRequestIQ and filters.idLength[IQ.Request](i) and filters.payload[IQ.Request](qn)) -> (qn.hashCode(), i)

    val stanzaIsRequestIQRosterQuery = stanzasWithIDandQN(XmppConstants.names.jabber.iq.roster.query)
    val stanzaIsRequestIQXmppBind = stanzasWithIDandQN(XmppConstants.names.urn.ietf.params.xmlNs.xmppBind.bind)

    val rules = Seq[Processor.Rule[Stanza, Any]](
      stanzaIsRequestIQ -> 1,
      stanzaIsResponseIQ -> 2,
      stanzaIsRequestMessage -> 3,
      stanzaIsErrorMessage -> 4,
      stanzaIsAvailabilityPresence -> 5,
      stanzaIsSubscriptionPresence -> 6,
      stanzaIsErrorPresence -> 7)

    val allRules = stanzaIsRequestIQRosterQuery ++ stanzaIsRequestIQXmppBind ++ rules

    val matrix = allRules.map(_._1).reduce(_ or _).nodes

    for {
      row <- matrix
    } {
      for { cell <- row } print("%s ".format(cell).padTo(30, ' '))
      println("")
    }

    val processor = NaiveProcessor.createProcessor(allRules)

    val inputs = Seq[(Stanza, Any)](
      IQ.request(Node(XmppConstants.names.jabber.iq.roster.query)).withId("1") -> (XmppConstants.names.jabber.iq.roster.query.hashCode(), 1),
      IQ.request(Node(XmppConstants.names.jabber.iq.roster.query)).withId("12") -> (XmppConstants.names.jabber.iq.roster.query.hashCode(), 2),
      IQ.request(Node(XmppConstants.names.jabber.iq.roster.query)).withId("123") -> (XmppConstants.names.jabber.iq.roster.query.hashCode(), 3),
      IQ.request(Node(XmppConstants.names.jabber.iq.roster.query)).withId("1234") -> (XmppConstants.names.jabber.iq.roster.query.hashCode(), 4),
      IQ.request(Node(XmppConstants.names.jabber.iq.roster.query)).withId("12345") -> (XmppConstants.names.jabber.iq.roster.query.hashCode(), 0),
      IQ.request(Node(XmppConstants.names.jabber.iq.roster.query)).withId("123456") -> (XmppConstants.names.jabber.iq.roster.query.hashCode(), 0),

      IQ.request(Node(XmppConstants.names.urn.ietf.params.xmlNs.xmppBind.bind)).withId("1") -> (XmppConstants.names.urn.ietf.params.xmlNs.xmppBind.bind.hashCode(), 1),
      IQ.request(Node(XmppConstants.names.urn.ietf.params.xmlNs.xmppBind.bind)).withId("12") -> (XmppConstants.names.urn.ietf.params.xmlNs.xmppBind.bind.hashCode(), 2),
      IQ.request(Node(XmppConstants.names.urn.ietf.params.xmlNs.xmppBind.bind)).withId("123") -> (XmppConstants.names.urn.ietf.params.xmlNs.xmppBind.bind.hashCode(), 3),
      IQ.request(Node(XmppConstants.names.urn.ietf.params.xmlNs.xmppBind.bind)).withId("1234") -> (XmppConstants.names.urn.ietf.params.xmlNs.xmppBind.bind.hashCode(), 4),
      IQ.request(Node(XmppConstants.names.urn.ietf.params.xmlNs.xmppBind.bind)).withId("12345") -> (XmppConstants.names.urn.ietf.params.xmlNs.xmppBind.bind.hashCode(), 0),
      IQ.request(Node(XmppConstants.names.urn.ietf.params.xmlNs.xmppBind.bind)).withId("123456") -> (XmppConstants.names.urn.ietf.params.xmlNs.xmppBind.bind.hashCode(), 0),

      IQ.result("1") -> 2,
      IQ.error("2", XmppStanzaError.ServiceUnavailable()) -> 2,

      Message.chat -> 3,
      Message.groupchat -> 3,
      Message.error(XmppStanzaError.FeatureNotImplemented()) -> 4,
      Presence.availability(PresenceType.Available) -> 5,
      Presence.subscription(PresenceType.Subscribe) -> 6,
      Presence.error(XmppStanzaError.ResourceConstraint()) -> 7 )

    inputs.foreach {
      case (stanza, result) =>
        processor.process(stanza) should contain (result)
    }


  }
}
