package provingground.translation
import edu.stanford.nlp.ling._
import edu.stanford.nlp.parser.lexparser._
import edu.stanford.nlp.process.{CoreLabelTokenFactory, PTBTokenizer, _}
import edu.stanford.nlp.tagger.maxent._
import edu.stanford.nlp.trees._
import provingground._
import provingground.interface.WordNet

import java.io._
import java.util
import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.util.matching.Regex

/**
  * Interface to the Stanford parser, handling (inline) TeX by separating tokenizing and POS tagging from parsing.
  * Parsing is done by the [[texParse]] method
  */
object StanfordParser {
  val lp: LexicalizedParser = LexicalizedParser.loadModel(
    "edu/stanford/nlp/models/lexparser/englishPCFG.ser.gz"
  )

  lazy val tlp: PennTreebankLanguagePack = new PennTreebankLanguagePack

  lazy val gsf: GrammaticalStructureFactory = tlp.grammaticalStructureFactory

  val tagger: MaxentTagger = new MaxentTagger(
    "edu/stanford/nlp/models/pos-tagger/english-left3words/english-left3words-distsim.tagger"
  )

  val tokenizerFactory: TokenizerFactory[CoreLabel] =
    PTBTokenizer.factory(new CoreLabelTokenFactory(), "")

  def coreLabels(s: String): util.List[CoreLabel] =
    tokenizerFactory.getTokenizer(new StringReader(s)).tokenize

  def words(s: String): mutable.Buffer[Word] =
    coreLabels(s).asScala map ((c) => new Word(c.word))

  def parse(s: String): Tree = lp(tagger(words(s).asJava))

  def texInline(s: String): Regex.MatchIterator =
    """\$[^\$]+\$""".r.findAllIn(s)

  def texDisplay(s: String): Regex.MatchIterator =
    """\$\$[^\$]+\$\$""".r.findAllIn(s)

  def reTag(word: String, tag: String)(tw: TaggedWord): TaggedWord =
    if (tw.word.toLowerCase == word) new TaggedWord(tw.word, tag) else tw

  // This method replaces a set of inline TeX expressions with the the TeX expression concatenated with "is true"
  def texPhraseReplacer(
      sentence: String,
      texExpressionIsPhrase: Iterable[Boolean]
  ): String = {
    texExpressionIsPhrase.zipWithIndex.foldLeft(sentence) {
      case (x, (b, i)) =>
        if (b)
          x.replace(s"TeXInline$i", s"TeXInline$i is true")
        else
          x
    }
  }

  def mergeTag(mwe: Vector[String], tag: String)(
      tws: Vector[TaggedWord]
  ): Vector[TaggedWord] =
    if (tws.take(mwe.size).map(_.word.toLowerCase) == mwe)
      (new TaggedWord(tws.take(mwe.size).map(_.word).mkString(" "), tag)) +: tws
        .drop(mwe.size)
    else
      tws match {
        case Vector() => Vector()
        case x +: ys  => x +: mergeTag(mwe, tag)(ys)
      }

  def mergeSubs(mwe: Vector[String], tw: TaggedWord)(
      tws: Vector[TaggedWord]
  ): Vector[TaggedWord] =
    if (tws.take(mwe.size).map(_.word.toLowerCase) == mwe)
      tw +: tws.drop(mwe.size)
    else
      tws match {
        case Vector() => Vector()
        case x +: ys  => x +: mergeSubs(mwe, tw)(ys)
      }

  def wordNetTags(
      tws: Vector[TaggedWord],
      depth: Int
  ): Vector[Vector[TaggedWord]] =
    tws match {
      case Vector() => Vector(Vector())
      case x +: ys =>
        val cdTag                    = if (x.word.startsWith("$")) Vector("CD") else Vector()
        def thinTags: Vector[String] = x.tag +: cdTag
        def fatTags: Vector[String] =
          (x.tag +: WordNet.posTags(x.word)) ++ cdTag
        def thinTailed: Vector[Vector[TaggedWord]] =
          for {
            tag  <- fatTags
            tail <- wordNetTags(ys, depth - 1)
            tw = new TaggedWord(x.word, tag)
          } yield tw +: tail
        def fatTailed =
          for {
            tag  <- thinTags
            tail <- wordNetTags(ys, depth)
            tw = new TaggedWord(x.word, tag)
          } yield tw +: tail
        if (depth > 1) thinTailed ++ fatTailed else fatTailed
    }

  case class TeXParsed(
      preraw: String,
      wordTags: Vector[(String, String)] = baseWordTags,
      mweSubs: Vector[(Vector[String], TaggedWord)] = baseMweSubs,
      texExpressionIsPhrase: Array[Boolean] = Array()
      // , mweTags: Vector[(Vector[String], String)] = Vector()
  ) {
    val raw: String = preraw
    // .replace("such that", "so that")
    // .replace("which", "where it") // useful for contituency parsing
    // .replace("that", "where it")

    lazy val texMap: Map[String, String] = (texInline(raw).zipWithIndex map {
      case (w, n) => (s"TeXInline$n", w)
    }).toMap

    lazy val deTeXed: String = texMap.foldRight(raw) {
      case ((l, w), s) => s.replace(w, l)
    }

    lazy val deTeXedWithReplacements:String = texPhraseReplacer(deTeXed, texExpressionIsPhrase)

    lazy val deTeXWords: mutable.Buffer[Word] = words(deTeXedWithReplacements)

    lazy val deTeXTagged: util.List[TaggedWord] = tagger(deTeXWords.asJava)

    def reTagged(tw: TaggedWord): TaggedWord =
      wordTags.foldRight(tw) { case ((w, tag), t) => reTag(w, tag)(t) }

    lazy val tagged: mutable.Buffer[TaggedWord] =
      deTeXTagged.asScala map { (tw) =>
        if (tw.word.startsWith("TeXInline"))
          new TaggedWord(texMap(tw.word), "NNP")
        else reTagged(tw)
      }

    lazy val mergeSubsTagged: Vector[TaggedWord] =
      mweSubs.foldRight(tagged.toVector) {
        case ((ws, tw), t) => mergeSubs(ws, tw)(t)
      }

    // lazy val mergeTagged =
    //     mweTags.foldRight(tagged.toVector){case ((ws, tag), t) => mergeTag(ws, tag)(t)}

    lazy val parsed: Tree = lp(mergeSubsTagged.asJava)

    lazy val polyParsed: Vector[Tree] =
      for {
        tws <- wordNetTags(mergeSubsTagged, 2)
      } yield lp(tws.asJava)

    lazy val gs: GrammaticalStructure = gsf.newGrammaticalStructure(parsed)

    lazy val tdl: util.List[TypedDependency] = gs.typedDependenciesCCprocessed

    import translation.NlpProse._

    def token(w: IndexedWord) = Token(w.word, w.index)

    lazy val typedDeps: mutable.Buffer[DepRel] =
      tdl.asScala.map { x =>
        DepRel(token(x.gov), token(x.dep), x.reln.toString)
      }

    lazy val proseTree: ProseTree = ProseTree(typedDeps.toList)
  }

  val baseWordTags =
    Vector("iff" -> "IN", "modulo" -> "IN")

  val assumptionTriggers: Vector[(Vector[String], TaggedWord)] =
    Vector(
      Vector("assume")                -> new TaggedWord("assume", "VB"),
      Vector("assume", "that")        -> new TaggedWord("assume", "VB"),
      Vector("now", "assume", "that") -> new TaggedWord("assume", "VB"),
      Vector("now", "assume", "for", "a", "contradiction", "that") -> new TaggedWord(
        "assume",
        "VB"
      ),
      Vector("assume", "for", "a", "contradiction", "that") -> new TaggedWord(
        "assume",
        "VB"
      ),
      Vector("suppose", "that")        -> new TaggedWord("assume", "VB"),
      Vector("now", "suppose", "that") -> new TaggedWord("assume", "VB")
//      Vector("consider") -> new TaggedWord("assume", "VB"),
//      Vector("now", "consider") -> new TaggedWord("assume", "VB"),
//      Vector("let") -> new TaggedWord("assume", "VB"),
//      Vector("now", "let") -> new TaggedWord("assume", "VB")
    )

  val baseMweSubs: Vector[(Vector[String], TaggedWord)] =
    Vector(
      Vector("if", "and", "only", "if") -> new TaggedWord("iff", "IN"),
      Vector("such", "that")            -> new TaggedWord("with", "IN")
    ) ++ assumptionTriggers

  def texParse(
      s: String,
      wordTags: Vector[(String, String)] = baseWordTags,
      // mweTags: Vector[(Vector[String], String)] = Vector(),
      mweSubs: Vector[(Vector[String], TaggedWord)] = baseMweSubs
  ): Tree =
    TeXParsed(s, wordTags, mweSubs).parsed

  def proseTree(
      s: String,
      wordTags: Vector[(String, String)] = baseWordTags,
      // mweTags: Vector[(Vector[String], String)] = Vector(),
      mweSubs: Vector[(Vector[String], TaggedWord)] = baseMweSubs
  ): NlpProse.ProseTree =
    TeXParsed(s, wordTags, mweSubs).proseTree

}
