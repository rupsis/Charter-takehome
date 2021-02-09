package test.poker

object Poker extends App {

  /*
   * Given a set of 5 playing card identifiers such as 2H, 7C, QS, 10D, 2D;
   * determine if this hand is better than some other hand, according to the rules of poker.
   *
   * Hands will be a string with 5 cards comma separated,
   * each card will have 1-2 digits or JQKA and a suit indicator C,D,S,H (i.e. 10C, KH)
   *
   * Possible Hand Types Below:
   *   Straight flush ex: (1C, 2C, 3C, 4C, 5C)
   *   Four of a kind ex: (1D, 1C, 1S, 1H, X)
   *   Full house ex: (1D, 1C, 1S, 2D, 2H)
   *   Flush  ex: (1D, 3D, KD, AD, 10D)
   *   Straight ex: (1C, 2D, 3H, 4C, 5S)
   *   Three of a kind ex: (1D, 1C, 1S, AD, 10D)
   *   Two pair ex: (1D, 1C, AC, AD, 10D)
   *   One pair ex: (1D, 1C, KD, AD, 10D)
   *
   * The goal of this is to compare between the hand types.
   * Comparing 2 of the same type (i.e. 2 straights) to determine a winner is outside the scope
   * and will not be tested.
   *
   * Implement hand1WinsOverHand2 method and return whether or not the first hand wins over the second hand.
   */

  // ordering suits / cards by highest value
  val cardSuits = Seq("C","D","H","S")
  val cardValues = Seq("2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K", "A")

  /*
  *  Card / Hand classes
  */

  case class Card(
    face: String,
    suit: String,
    value: Double
  )

  case class Hand(
    cards: Seq[Card],
    value: Double
  )

  /*
  *  Card / Hand parsing method
  */

  def parseCard(card: String): Card = {
    if (!cardSuits.contains(card.takeRight(1))){
      throw new RuntimeException("card must be a valid suit (C,D,S,H)")
    }

    if (!cardValues.contains(card.dropRight(1))){
      throw new RuntimeException("card must a valid value (2-10, J, Q, K, A) No jokers. ")
    }

    val cardSuit = card.takeRight(1)
    val cardFace = card.dropRight(1)

  // set the cards value based on it's face + suit
    Card(
      face = cardFace,
      suit = cardSuit,
      value = (cardValues.indexOf(cardFace) + 1) + ((cardSuits.indexOf(cardSuit) + 1) * 0.1)
    )
  }


  def parseHand(hand: String): Hand = {
   val cards = hand.split(",")

   if(cards.length != 5) throw new RuntimeException("hand must contain only 5 cards")

    Hand(
      cards = cards.map(card => parseCard(card)),
      value = 0
    )
  }

  /*
  *  Hand evaluation methods. 3 baisc types, Straigh, Flush, and some pair (triple, 2 pair, etc)
  */

  def isStraigh(cards: Seq[Card]): Boolean = {
    val cardFaces = cards.map(card => cardValues.indexOf(card.face))
    val cardSet = cardFaces.toSet
    // There is one case where the straight (baby straight) can be ace, 2, 3, 4, 5
    cardSet.size == 5 && ((cardSet, cardSet.drop(1)).zipped.forall ((l,r) => (r-l) == 1) || cardSet == Set(12, 0, 1, 2, 3))
  }

  def isFlush(cards: Seq[Card]): Boolean = cards.map(_.suit).toSet.size == 1

  def isMultipleKind(cards: Seq[Card], groupSize: Long, occurances: Long = 1) = {
    cards.groupBy(_.face).values.count(_.size == groupSize) == occurances
  }


//   Really not a huge fan of this pattern... But it works
  def handValue(hand: Hand): Double = {

    if(isStraigh(hand.cards) && isFlush(hand.cards)) return 1000

    if(isMultipleKind(hand.cards, 4, 1)) return 900

    if(
      isMultipleKind(hand.cards, 3, 1) &&
      isMultipleKind(hand.cards, 2, 1)
    ) return 800

    if(isFlush(hand.cards)) return 700

    if(isStraigh(hand.cards)) return 600

    if(isMultipleKind(hand.cards, 3, 1)) return 500

    if(isMultipleKind(hand.cards, 2, 2)) return 400

    if(isMultipleKind(hand.cards, 2, 1)) return 300

    // if no matched hand in play, return high card value
    hand.cards.reduceLeft(
      (c1: Card, c2: Card) => if(c1.value > c2.value) c1 else c2
    ).value
  }


  /*
  * First, parse the hand into a useable data structure.
  * Second, match against poker hands to determine value
  * Finally compare the two hands, and declare a winner
  */
  def hand1WinsOverHand2(hand1Str: String, hand2Str: String): Boolean = {
    val hand1: Hand = parseHand(hand1Str)
    val hand2: Hand = parseHand(hand2Str)

//   ensure hands are unique
    println()
    if(!(hand1.cards ++ hand2.cards).groupBy(
      card => (card.face, card.suit)).values.forall(_.size == 1)
    ){
      throw new RuntimeException("Invalid hand, contains duplicate cards")
    }
    handValue(hand1) > handValue(hand2)
  }

  implicit class CompareTwoPokerHands(hand1: String) {
    def winsOver(hand2: String): Unit = {
      val result = if (hand1WinsOverHand2(hand1, hand2)) "Correct" else "Incorrect"
      println(s"$result, hand [$hand1] wins over [$hand2]")
    }
  }

  println("Poker Hand comparison")
  "8C,9C,10C,JC,QC" winsOver "6S,7H,8D,9H,10D" // straight flush
  "4H,4D,4C,4S,JS" winsOver "6C,6S,KH,AS,AD" // four of a kind
  "5C,3C,10C,KC,7C" winsOver "6C,6D,6H,9C,KD" // flush
  "4H,4D,4C,KC,KD" winsOver "9D,6S,KH,AS,AD" // full house
  "2C,3C,4S,5S,6S" winsOver "6C,6D,6H,9C,KD" // straight
  "7C,7D,7S,3H,4D" winsOver "9S,6S,10D,AS,AD" // three of a kind
  "8C,8H,10S,KH,KS" winsOver "2S,2D,JH,7S,AC" // two pair
  "AC,AH,3C,QH,10C" winsOver "3S,2D,KH,JS,AD" // one pair
  "AC,2H,3C,QH,10C" winsOver "3S,2D,KH,JS,5D" // high card

  Seq(
    ("17C,AH,3C,QH,10C", "3S,2D,KH,JS,AD"), // bad card value
    ("17C,AH,3C,QH", "3S,2D,KH,JS,AD"),   // bad hand length
    ("10C,AH,3C,QH,2Z", "3S,2D,KH,JS,AD"), // bad vard suit
    ("AC,AH,3C,QH,10C", "AC,2D,KH,JS,AD") // duplicate cards
  ).map(x => {
    try{
     x._1 winsOver x._2
    } catch {
      case e: RuntimeException => println(s"invalid hand: $e")
    }
  });


  System.exit(0)
}
