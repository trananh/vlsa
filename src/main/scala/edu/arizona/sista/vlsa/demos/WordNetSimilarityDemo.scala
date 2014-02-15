package edu.arizona.sista.vlsa.demos

import edu.cmu.lti.lexical_db.{ILexicalDatabase, NictWordNet}
import edu.cmu.lti.ws4j.RelatednessCalculator
import edu.cmu.lti.ws4j.impl._
import edu.cmu.lti.ws4j.util.WS4JConfiguration

/** Wordnet similarity for Java (ws4j) demo.
  * https://code.google.com/p/ws4j/
  */
object WordNetSimilarityDemo {

  val db: ILexicalDatabase = new NictWordNet()
  val rcs: Array[RelatednessCalculator] = Array(new HirstStOnge(db),
    new LeacockChodorow(db), new Lesk(db), new WuPalmer(db), new Resnik(db),
    new JiangConrath(db), new Lin(db), new Path(db))

  def run(word1: String, word2: String) {
    // True = only use the most frequent sense, saves some computation time.
    WS4JConfiguration.getInstance.setMFS(false)
    for (rc <- rcs) {
      val s: Double = rc.calcRelatednessOfWords(word1, word2)
      System.out.println(rc.getClass.getName + "\t" + s)
    }
  }

  def main(args: Array[String]) {
    val t0: Long = System.currentTimeMillis
    run("startled", "surprised")
    val t1: Long = System.currentTimeMillis
    System.out.println("Done in " + (t1 - t0) + " msec.")
  }

}