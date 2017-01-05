package forcomp


object Anagrams {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[(Char, Int)]

  /** The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = loadDictionary

  /** Converts the word into its character occurrence list.
   *
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   *
   *  Note: you must use `groupBy` to implement this method!
   */
  def wordOccurrences(w: Word): Occurrences = List(w.toLowerCase).flatten.groupBy(ch => ch).mapValues(x => x.size).toList.sortBy(_._1)

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = wordOccurrences(s.foldLeft("")(_ ++ _))

  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *
   *  For example, the word "eat" has the following character occurrence list:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] =
    dictionary.map(w => (wordOccurrences(w), w)).groupBy(x => x._1).mapValues(x => x.map(a => a._2))

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences(wordOccurrences(word))

  /** Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   *  is a subset of `List(('k', 1), ('o', 1))`.
   *  It also include the empty subset `List()`.
   *
   *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Note that the order of the occurrence list subsets does not matter -- the subsets
   *  in the example above could have been displayed in some other order.
   */
  def combinations(occurrences: Occurrences): List[Occurrences] = {
    def loop(t: Occurrences, acc: List[Occurrences]): List[Occurrences] = {
      if(t.isEmpty)
        acc
      else {
        val v = t.head
        val res1 = acc :+ List(v)
        val res2 = acc.filter(x => !x.isEmpty && !x.exists(y => y._1.equals(v._1))).map(x => x :+ v)

        if(res2.size > 0)
          loop(t.tail, res1 ::: res2)
        else
          loop(t.tail, res1)
      }
    }

    val t = for {
      (ch, occ) <- occurrences
      i <- 1 to occ
    } yield (ch, i)

    val acc = List[Occurrences](List())
    loop(t, acc)
  }

  /** Subtracts occurrence list `y` from occurrence list `x`.
   *
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    if(y.isEmpty) {
      x
    }else {
      val res = x.map(v => {
        if(v._1.equals(y.head._1)) {
          val diff = v._2 - y.head._2
          if(diff > 0)
            (v._1, diff)
          else
            Nil
        } else
          v
      }).filter(x => x != Nil)
      subtract(res.asInstanceOf[Occurrences], y.tail)
    }
  }

  /** Returns a list of all anagram sentences of the given sentence.
   *
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
   */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    val senOccur = sentenceOccurrences(sentence)
    val totalLen = sentence.map(x => x.size).foldLeft(0)((x, y) => x + y)

    def loop(occur: Occurrences, buffer: List[Word], acc2: List[Sentence]): List[Sentence]= {
      def retrieveOccurences(comb: List[Occurrences], acc: Map[Occurrences, List[Word]]): Map[Occurrences, List[Word]] = {
        if (comb.isEmpty)
          acc
        else {
          val head = dictionaryByOccurrences.get(comb.head)
          head match {
            case Some(words) => retrieveOccurences(comb.tail, acc + (comb.head -> head.get))
            case None => retrieveOccurences(comb.tail, acc)
          }
        }
      }

      val comb = combinations(occur)
      val mapOccWord = retrieveOccurences(comb, Map[Occurrences, List[Word]]())

      def retrieveSentence(keySet: Set[Occurrences], acc2: List[Sentence]): List[Sentence] = {
        def retrieveWords(senOccur2: Occurrences, words: List[Word], acc2: List[Sentence]): List[Sentence] = {
          if (words.isEmpty)
            acc2
          else {
            val word = words.head
            val temp = buffer :+ word
            val len = temp.flatten.size
            if (senOccur2.size == 0 && len == totalLen)
              retrieveWords(senOccur2, words.tail, acc2 :+ temp)
            else {
              val res = retrieveWords(senOccur2, words.tail, acc2)
              loop(senOccur2, buffer :+ word, res)
            }
          }
        }

        if(keySet.isEmpty)
          acc2
        else {
          val key = keySet.head
          val senOccur2 = subtract(occur, key)
          val elem = mapOccWord.get(key).get
          val res = retrieveWords(senOccur2, elem, acc2)
          retrieveSentence(keySet.tail, res)
        }
      }

      val keySet = mapOccWord.keySet
      retrieveSentence(keySet, acc2)
    }

    val result = loop(senOccur, List[Word](), List[Sentence]())
    if(result.size == 0)
      List(Nil)
    else
      result
  }
}
