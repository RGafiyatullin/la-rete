package tests

import com.github.rgafiyatullin.la_rete.{Filter, Property}
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
        override def apply(obj: Stanza): S =
          obj match {
            case _: IQ => S.I
            case _: Presence => S.P
            case _: Message => S.M
          }
        override def isDefinedAt(obj: Any): Boolean = obj.isInstanceOf[Stanza]
      }

      object iq extends Property[IQ, IQType] {
        override def apply(obj: IQ): IQType = obj.iqType
        override def isDefinedAt(obj: Any): Boolean = obj.isInstanceOf[IQ]
      }

      object message extends Property[Message, MessageType] {
        override def apply(obj: Message): MessageType = obj.messageType
        override def isDefinedAt(obj: Any): Boolean = obj.isInstanceOf[Message]
      }

      object presence extends Property[Presence, PresenceType] {
        override def apply(obj: Presence): PresenceType = obj.presenceType
        override def isDefinedAt(obj: Any): Boolean = obj.isInstanceOf[Presence]
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
  }
}
