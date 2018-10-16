package tests

import com.github.rgafiyatullin.la_rete.{Filter, Processor}
import com.github.rgafiyatullin.la_rete.processors.naive.NaiveProcessor
import com.github.rgafiyatullin.la_rete.processors.trie.TrieProcessor
import com.github.rgafiyatullin.xmpp_protocol.stanzas.Stanza
import com.github.rgafiyatullin.xmpp_protocol.stanzas.iq.IQ
import com.github.rgafiyatullin.xmpp_protocol.stanzas.message.Message

final class DuplicateRulesTest extends TestBase {
  "duplicate rules" should "still work (#1)" in {
    val matches = new Matches
    import matches.filters

    val rules = Seq[(Filter[Stanza, _], Int)](
        filters.stanza.isIQ -> 1
      , (filters.stanza.isIQ and filters.iq.isResponse) -> 2 // this rule is "shaded", i.e. will never match
      , filters.stanza.isMessage -> 3
    )

    def run(p: Processor[Stanza, Int]): Unit = {
      p.process(IQ.result("1")) should be (Some(1))
      p.process(Message.chat) should be (Some(3))
      ()
    }

    run(NaiveProcessor.createProcessor[Stanza, Int](rules))
    run(TrieProcessor.createProcessor[Stanza, Int](rules))
  }
}
