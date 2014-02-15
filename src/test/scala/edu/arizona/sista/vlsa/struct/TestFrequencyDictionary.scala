package edu.arizona.sista.vlsa.struct

import edu.arizona.sista.vlsa.math.Stats
import junit.framework.Assert._
import org.junit.{Test, Before, After}
import org.scalatest.junit.AssertionsForJUnit
import scala.Predef._
import scala.collection.mutable.ListBuffer

/** Tests for FrequencyDictionary.
  *
  * @author trananh
  */
class TestFrequencyDictionary extends AssertionsForJUnit {

  val terms = Array("Term1", "Term2", "Term3")
  var dictionary: FrequencyDictionary[String] = _
  val N = 3

  @Before
  def setUp() {
    dictionary = new FrequencyDictionary[String](Option(terms), N = N)
  }

  @After
  def tearDown() {
    dictionary.clear()
  }

  @Test
  def testInitialization() {
    // All entries should be initialized with frequency 0
    assertEquals(dictionary.freq(0).keys.size, terms.size)
    assertEquals(dictionary.attributes(0).keys.size, terms.size)
    terms.foreach(t => {
      assertTrue(dictionary.freq(0).contains(List(t)))
      assertEquals(dictionary.freq(0)(List(t)), 0)
      assertEquals(dictionary.getCount(List(t)), 0)
    })
  }

  @Test
  def testCreateEntry() {
    // New entry should be properly added with frequency 0
    assertTrue(!dictionary.freq(0).contains(List("NewTest1")))
    dictionary.createEntry("NewTest1")
    assertTrue(dictionary.freq(0).contains(List("NewTest1")))
    assertEquals(dictionary.freq(0)(List("NewTest1")), 0)
    assertEquals(dictionary.getCount(List("NewTest1")), 0)
    val oldSize = dictionary.freq(0).keys.size

    // Adding an existing entry shouldn't change anything
    dictionary.createEntry("NewTest1")
    assertEquals(dictionary.freq(0).keys.size, oldSize)
    assertEquals(dictionary.attributes(0).keys.size, oldSize)
  }

  @Test
  def testAddEntry() {
    // Generate some repeating new terms
    val newTerms = Array("NewTerm1", "NewTerm2", "NewTerm3")
    val repeats = new ListBuffer[String]()
    for (i <- 1 to newTerms.size) {
      for (j <- 0 until i) {
        repeats.append(newTerms(i-1))
      }
    }

    // Adding new terms should add or increment them
    for (k <- 1 to 2) {
      dictionary.addEntries(repeats, Option("Passage " + k))
      for (i <- 1 to N) {
        val combos = Stats.combinations(i, repeats.toList).toList
        val counts = combos.map(x => combos.count(y => y.equals(x)))
        for (j <- 0 until combos.size) {
          assertEquals(dictionary.freq(i-1)(combos(j)), k * counts(j))
          assertEquals(dictionary.getCount(combos(j)), k * counts(j))
          assertEquals(dictionary.getAttributes(combos(j))(k * counts(j) - 1).asInstanceOf[String], "Passage " + k)
        }
      }
    }

  }

  @Test
  def testIncrement() {
    // Incrementing a bogus entry does nothing
    val newTerms = Array("NewTerm1", terms(0), "NewTerm3")
    dictionary.increment(newTerms)
    dictionary.freq.foreach(d => {
      d.keys.foreach(k => if (k.equals(List(terms(0)))) assertEquals(d(k), 1) else assertEquals(d(k), 0))
    })

    // Reset states
    tearDown()
    setUp()

    // Incrementing univariate entries by 1
    for (i <- 0 until terms.size) {
      for (j <- 0 until i) {
        dictionary.increment(List(terms(i)), Option("Passage" + j + terms(i)))
        assertEquals(dictionary.getCount(List(terms(i))), j + 1)
        assertEquals(dictionary.getAttributes(List(terms(i)))(j).asInstanceOf[String], "Passage" + j + terms(i))
      }
    }

    // Returned list should be properly sorted
    var total = 0f
    val sortedList = dictionary.toSortedList(1).reverse
    for (i <- 0 until terms.size) {
      assertEquals(sortedList(i)._1, List(terms(i)))
      assertEquals(sortedList(i)._2, i)
      total += sortedList(i)._2
    }

    // Returned normalized list should be properly calculated and sorted
    val norm = dictionary.toSortedListNorm(1).reverse
    for (i <- 0 until terms.size) {
      assertEquals(norm(i)._1, List(terms(i)))
      assertEquals(norm(i)._2, i / total)
    }
  }

  @Test
  def testReset() {
    testAddEntry()
    for (i <- 0 until N) {
      var sum = 0f
      dictionary.freq(i).values.foreach(v => sum += v)
      assertEquals(dictionary.total(i), sum)
    }

    dictionary.reset()
    dictionary.freq.foreach(f => f.foreach(kv => assertEquals(f(kv._1), 0)))
    dictionary.attributes.foreach(a => a.foreach(kv => assertEquals(a(kv._1).size, 0)))
    dictionary.total.foreach(t => assertEquals(t, 0f))
  }

  @Test
  def testGetAttribute() {
    val passages = Array("Passage0", "Passage1", "Passage2")
    assertEquals(dictionary.getAttributes(List("Bogus")).size, 0)
    passages.foreach(p => dictionary.increment(terms, Option(p)))
    assertEquals(dictionary.getAttributes(terms.slice(0, 1).toList).mkString(","), passages.mkString(","))
    assertEquals(dictionary.getAttributes(terms.slice(0, 2).toList).mkString(","), passages.mkString(","))
    assertEquals(dictionary.getAttributes(terms.toList).mkString(","), passages.mkString(","))
  }

}
