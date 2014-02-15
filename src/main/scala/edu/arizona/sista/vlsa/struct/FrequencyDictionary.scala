package edu.arizona.sista.vlsa.struct

import edu.arizona.sista.vlsa.math.Stats
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/** A simple frequency dictionary structure to track the occurrence count of
  * key entry values, as well as the joint-occurrence count of pairs of values.
  *
  * @constructor A frequency dictionary to keep counts of key entry values.
  * @param initEntries Initial list of entries.
  * @param N Maximum size of n-element joint distributions to track
  *          (Default to 1, which means keep just the frequency count of entries
  *          and ignore joint occurrences).
  * @tparam T Type of data to store in the dictionary and keep frequency count for.
  *
  * @author trananh
  */
class FrequencyDictionary[T <: Comparable[T]](initEntries: Option[Iterable[T]] = None,
    var N: Integer = 1) {

  /** Keep the frequency counts at various levels in order to track n-element joint frequencies */
  var freq = new ListBuffer[mutable.HashMap[List[T], Integer]]()

  /** Keep additional attributes associated with each entry */
  var attributes = new ListBuffer[mutable.HashMap[List[T], ListBuffer[Any]]]()

  /** Track the total number of occurrences across all entries */
  var total = new ListBuffer[Float]

  /** Initialize the frequency tables */
  N = math.max(N, 1)
  for (i <- 0 until N) {
    freq.append(new mutable.HashMap[List[T], Integer]())
    attributes.append(new mutable.HashMap[List[T], ListBuffer[Any]])
    total.append(0f)
  }

  /** Load tables with initial entries */
  if (initEntries.isDefined)
    loadEntries(initEntries.get)

  /** Make a deep copy of the dictionary.
    * @return A deep copy of the dictionary.
    */
  override def clone(): FrequencyDictionary[T] = {
    val copy = new FrequencyDictionary[T](N = N)
    for (i <- 0 until N) {
      copy.freq(i) = this.freq(i).clone()
      copy.attributes(i) = this.attributes(i).clone()
      copy.total(i) = this.total(i)
    }
    copy
  }

  /** Clear the dictionary contents, but maintain the internal structure. */
  def clear() {
    for (i <- 0 until N) {
      freq(i).clear()
      attributes(i).clear()
      total(i) = 0f
    }
  }

  /** Reset frequency. Set all counts to 0 and drop all previously added attributes. */
  def reset() = {
    freq.foreach(f => f.foreach(kv => f(kv._1) = 0))
    attributes.foreach(a => a.foreach(kv => a(kv._1).clear()))
    for (i <- 0 until total.size) total(i) = 0f
  }

  /** Load list of univariate entries and initialize their frequencies to 0 */
  def loadEntries(values: Iterable[T]) {
    values.foreach(e => createEntry(e))
  }

  /** Add a new univariate entry with frequency 0. */
  def createEntry(value: T) {
    // For each new entry, we will add it to the bottom (univariate) level,
    // but we won't add the pairing combinations to the multivariate levels until needed.
    if (!freq(0).isDefinedAt(List(value))) {
      freq(0).put(List(value), 0)
      attributes(0).put(List(value), new ListBuffer[Any]())
    }
  }

  /** Increment the frequency and joint frequency counts for the
    * given values and add them as new entries if they don't exist.
    * Examples:
    *   - increment(List("hello", "world")) => increment frequency for "hello", "world", and the joint
    *       frequency for "hello" & "world".
    *
    * @param values The entry values to process counts for.
    * @param attribute Attribute to associate with the specified values.
    */
  def addEntries(values: Iterable[T], attribute: Option[Any] = None) {
    for (n <- 1 to math.min(N, values.size)) {
      val combos = Stats.combinations(n, values.toList)
      combos.foreach(c => {
        val v = c.toList.sorted
        if (!freq(n-1).isDefinedAt(v)) {
          // Add the entry if not exist
          freq(n-1).put(v, 0)
          attributes(n-1).put(v, new ListBuffer[Any]())
        }
        // Increment
        freq(n-1)(v) += 1
        total(n-1) += 1
        if (attribute.isDefined)
          attributes(n-1)(v).append(attribute.get)
      })
    }
  }

  /** Increment the frequency and joint frequency counts for the
    * given values, if they exist.
    * Examples:
    *   - increment(List("hello", "world")) => increment frequency for "hello", "world", and the joint
    *       frequency for "hello" & "world", assuming both "hello" and "world" are valid entries.
    * @param values The entry values to process counts for.
    * @param attribute Attribute to associate with the specified values.
    */
  def increment(values: Iterable[T], attribute: Option[Any] = None) {
    val validValues = values.filter(v => freq(0).keySet.contains(List(v)))
    addEntries(validValues, attribute)
  }

  /** Get the frequency or joint frequency count for the entry.
    * Examples:
    *   - getCount("hello") => frequency count of "hello"
    *   - getCount(List("hello", "world")) => joint frequency count of "hello" and "world"
    *
    * @param key The key entry to look up normalized frequency for.
    * @return The frequency associated with the key entry.
    */
  def getCount(key: List[T]): Integer = {
    val kList = key.sorted
    if (key.size <= N && attributes(key.size-1).contains(kList))
      return freq(key.size - 1)(kList)
    0
  }

  /** Get the frequency or joint frequency (normalized by the total) for the entry.
    * Examples:
    *   - getPercentage("hello") => distribution of "hello" normalized by count of all other uni-grams.
    *   - getCount(List("hello", "world")) => joint distribution normalized by count of all other bi-grams.
    *
    * @param key The key entry to look up normalized frequency for.
    * @return The frequency associated with the key entry.
    */
  def getPercentage(key: List[T]): Float = {
    if (key.size <= N) getCount(key) / total(key.size - 1) else 0f
  }

  /** Return all attributes associated with the entry.
    * @param key The key entry to look up attributes for.
    * @return All attributes associated with the key entry.
    */
  def getAttributes(key: List[T]): Array[Any] = {
    val kList = key.sorted
    if (key.size <= N && attributes(key.size-1).contains(kList))
      return attributes(key.size-1)(kList).toArray[Any]
    Array.empty[Any]
  }

  /** Return set of all n-element tokens and their counts.
    * @param n Size of the joint entries to return (default to 1).
    * @return Set of n-element tokens and their counts.
    */
  def getAllTokenCounts(n: Integer = 1): mutable.HashMap[List[T], Integer] = {
    val level = math.max(n, 1)
    if (level > freq.length)
      return mutable.HashMap[List[T], Integer]()
    freq(level-1).clone()
  }

  /** Return set of all n-element tokens that occurred at least once, and their associated counts.
    * @param n Size of the joint entries to return (default to 1).
    * @return Set of n-element tokens and their counts.
    */
  def getOccurringTokenCounts(n: Integer = 1): mutable.HashMap[List[T], Integer] = {
    val level = math.max(n, 1)
    if (level > freq.length)
      return mutable.HashMap[List[T], Integer]()
    freq(level-1).filter(entry => entry._2 > 0).clone()
  }

  /** Return set of all n-element tokens.
    * @param n Size of the joint entries to return (default to 1).
    * @return Set of n-element tokens.
    */
  def getAllTokens(n: Integer = 1): scala.collection.Set[List[T]] = getAllTokenCounts(n).keySet

  /** Return set of all n-element tokens that occurred at least once.
    * @param n Size of the joint entries to return (default to 1).
    * @return Set of n-element tokens.
    */
  def getOccurringTokens(n: Integer = 1): scala.collection.Set[List[T]] = getOccurringTokenCounts(n).keySet

  /** Return list of n-element joint entries with the frequency counts,
    * sorted by frequency.
    * @param n Size of the joint entries to return (default to 1).
    * @return List of n-element joint entries with the frequency.
    */
  def toSortedList(n: Integer = 1): List[(List[T], Integer)] = {
    val level = math.max(n, 1)
    if (level > freq.length)
      return List[(List[T], Integer)]()
    freq(level-1).toList.sortWith(_._2 > _._2)
  }

  /** Return list of n-element joint entries with the normalized frequency
    * percentage, sorted by frequency.
    * @param n Size of the joint entries to return (default to 1).
    * @return List of n-element joint entries with the normalized frequency.
    */
  def toSortedListNorm(n: Integer = 1): List[(List[T], Float)] = {
    toSortedList(n).map(x => (x._1, x._2 / total(n-1)))
  }

  /** Format the contents of the dictionary into a nice string for display.
    * @param delim Delimiter used to separate each dictionary entry.
    * @return A nice formatted string of the dictionary contents.
    */
  def prettyPrint(delim: String = ", "): String = {
    val sb = new StringBuilder()
    for (i <- 1 to N) {
      if (!freq(i-1).toList.isEmpty) {
        sb.append(i + "-grams:\n")
        sb.append(toSortedList(i).mkString(delim) + "\n")
        sb.append(toSortedListNorm(i).mkString(delim) + "\n")
      }
    }
    sb.toString()
  }

}
