package crossword

import scala.io.StdIn
import scala.util.Random

/**
  * Created by Sinto on 16/08/2020.
  */
object Solution {

  //find all candidates words based on length of slot
  def findCandidateWords(wordSlots: List[WordSlot], inputWords: List[String]): List[Candidates] =
    wordSlots map (wslot => Candidates(wslot.slotId, wslot.direction, inputWords.filter(_.size == wslot.slot.length)))

  //find Intersection points
  def findIntersections(slots: List[WordSlot]): List[IntersectionPoint] =
    for {
      hSlot <- slots.filter(_.direction == HORIZONTAL)
      vSlot <- slots.filter(_.direction == VERTICAL)
      intersec = hSlot.slot.filter(point => vSlot.slot.contains(point))
      if !intersec.isEmpty
    } yield {
      val intersectionPoint = intersec.head;
      IntersectionPoint(
        hSlot.slotId,
        vSlot.slotId,
        intersectionPoint,
        hSlot.slot.indexOf(intersectionPoint),
        vSlot.slot.indexOf(intersectionPoint)
      )
    }

  //find all word slots
  def getSlots(matrix: Array[Array[Char]]) = {
    val hPlaceHolders = List.range(0, 10) map
      (row => List.range(0, 10) map (col => (row, col)) filter (tup => matrix(tup._1)(tup._2) == '-')) filter
      (_.size > 1)
    val vPlaceHolders = List.range(0, 10) map
      (col => List.range(0, 10) map (row => (row, col)) filter (tup => matrix(tup._1)(tup._2) == '-')) filter
      (_.size > 1)
    val hSlots        = cleanSlots(hPlaceHolders, HORIZONTAL).filter(_.size > 1)
    val vSlots        = cleanSlots(vPlaceHolders, VERTICAL).filter(_.size > 1)

    val listOfHWordSlots = hSlots
      .zipWithIndex
      .map {
        case (slot, index) =>
          WordSlot(s"H$index", slot, HORIZONTAL)
      }
    val listOfVWordSlots = vSlots
      .zipWithIndex
      .map {
        case (slot, index) =>
          WordSlot(s"V$index", slot, VERTICAL)
      }
    listOfHWordSlots ++ listOfVWordSlots
  }

  //clean the slots to get multiple separate slots in one line(horizontal or vertical)
  def cleanSlots(
      slots: List[List[(Int, Int)]],
      direction: Direction,
      acc: List[List[(Int, Int)]] = List.empty
  ): List[List[(Int, Int)]] =
    slots match {
      case Nil    =>
        acc
      case h :: t =>
        cleanSlots(t, direction, acc ++ splits(h, direction))
    }

  def closeEnough(a: Int, b: Int) = Math.abs(b - a) == 1

  //splits the placeholder sequence if they are not continuous in a row or column
  def splits(list: List[(Int, Int)], direction: Direction): List[List[(Int, Int)]] = {
    val revList = list.reverse
    revList
      .drop(1)
      .foldLeft(List(List(revList.head)))((acc, e) =>
        if (
          (
            direction == HORIZONTAL && closeEnough(e._2, acc.head.head._2) ||
            (direction == VERTICAL && closeEnough(e._1, acc.head.head._1))
          )
        )
          (e :: acc.head) :: acc.tail
        else
          List(e) :: acc
      )
  }

  def printMatrix(matrix: Array[Array[Char]], size: Int) = println(matrix.map(_.mkString).mkString("\n"))

  /**
    *
    * @param originalCandidateList keeps original full candidate list to check cross matches in interation for words
    * @param candidatesList drops the candidate already fitted in each iteration
    * @param intersectionPts
    * @param invalidSlots keeps track of invalid slots
    * @param usedWords keeps track to used words to avoid reuse
    * @param fits the resulting list
    * @return (slotId,fit-word) for every wordSlot
    */
  def findFits(
      originalCandidateList: List[Candidates],
      candidatesList: List[Candidates],
      intersectionPts: List[IntersectionPoint],
      invalidSlots: List[String] = List.empty,
      usedWords: List[(String, Direction)] = List.empty,
      fits: List[(String, String)] = List.empty
  ): List[(String, String)] =
    candidatesList match {
      case Nil    =>
        fits
      case h :: t =>
        val fittedWords =
          for {
            word <- h.fittableWords.filter(word => !usedWords.exists(x => x._1 == word))
            crossMatch =
              checkForCrossMatch(
                originalCandidateList,
                intersectionPts.filterNot(x => invalidSlots.contains(x.hslotId) || invalidSlots.contains(x.vslotId)),
                word,
                h.slotId,
                h.direction,
                usedWords
              )
            if (!crossMatch.isEmpty) && (crossMatch forall (_ == true))
          } yield (word, h.direction)
        if (fittedWords.isEmpty)
          findFits(originalCandidateList, t, intersectionPts, invalidSlots :+ h.slotId, usedWords, fits)
        else {
          //randomly attempting matching fits as the number of multiple matching fits will be very less.
          val selectedFittedWord = fittedWords(new Random().nextInt(fittedWords.length))
          findFits(
            originalCandidateList,
            t,
            intersectionPts,
            invalidSlots,
            usedWords :+ (selectedFittedWord._1, selectedFittedWord._2),
            fits :+ (h.slotId, selectedFittedWord._1)
          )
        }
    }

  /*checks cross match for every word. Before placing a word in a position it checks all intersections and checks
   if the intersecting characters are same based on horizontal Index and vertica Index. It also
   */
  def checkForCrossMatch(
      candidatesList: List[Candidates],
      intersectionPts: List[IntersectionPoint],
      currWord: String,
      currSlotId: String,
      currDirection: Direction,
      usedWords: List[(String, Direction)]
  ) = {
    val crossMatchCandidates = candidatesList
      .filter(candidates => candidates.direction != currDirection && !candidates.fittableWords.isEmpty)
    for {
      intersec         <-
        intersectionPts.filter(point =>
          currDirection match {
            case HORIZONTAL =>
              currSlotId == point.hslotId
            case VERTICAL   =>
              currSlotId == point.vslotId
          }
        )

      crossMatchResult <-
        crossMatchCandidates
          .filter(c =>
            currDirection match {
              case HORIZONTAL =>
                c.slotId == intersec.vslotId
              case VERTICAL   =>
                c.slotId == intersec.hslotId
            }
          )
          .map { crossMatchCandidate =>
            val fittableInSelectedinCurrDir = crossMatchCandidate
              .fittableWords
              .filter(word => usedWords.exists(x => (x._1 == word) && (x._2 == currDirection)))
            val candidatePruned             = crossMatchCandidate
              .fittableWords
              .filter(_ != currWord)
              .filter(x => !(fittableInSelectedinCurrDir.contains(x)))
            val matchResult                 = candidatePruned.map(fittableWord =>
              currDirection match {
                case HORIZONTAL =>
                  currWord(intersec.hIndex) == fittableWord(intersec.vIndex)
                case VERTICAL   =>
                  currWord(intersec.vIndex) == fittableWord(intersec.hIndex)
              }
            )
            matchResult exists (_ == true)
          }

    } yield crossMatchResult
  }

  /* replaces slots in matrix with actual word */
  def replace(inputMatrix: Array[Array[Char]], slots: List[(Int, Int)], word: Option[String]): Unit =
    word match {
      case None       =>
        ()
      case Some(word) =>
        slots
          .zipWithIndex
          .map {
            case (slot, index) =>
              inputMatrix(slot._1)(slot._2) = word(index)
          }
    }

  /*read back the fitted words to compare with input to decide whether the fit is success.*/
  def readFits(wordFits: Map[String, String], matrix: Array[Array[Char]], wordSlots: List[WordSlot]): List[String] = {
    val validSlotIds = wordFits.toList.map(_._1)
    for {
      validWordSlots <- wordSlots.filter(wordSlot => validSlotIds.contains(wordSlot.slotId))
      word =
        for {
          slots <- validWordSlots.slot
        } yield (matrix(slots._1)(slots._2))
    } yield word.mkString
  }

  /**Fits the matrix with found matches for word slots and then reads back the fitted words from valid slots
    *
  * @return Returns true if all '-' is replaced & all the words inslot are present in input set of words.
    * */
  def fitMatrix(
      inputMatrix: Array[Array[Char]],
      wordFits: Map[String, String],
      wordSlots: List[WordSlot],
      inputWords: List[String]
  ): Boolean = {
    wordSlots map (wordSlot => replace(inputMatrix, wordSlot.slot, wordFits.get(wordSlot.slotId)))
    val fittedWords = readFits(wordFits, inputMatrix, wordSlots)
    inputMatrix.filter(_.contains('-')).size == 0 && (fittedWords.forall(inputWords.contains(_)))
  }

  /**
    * This is the control method for the crossword fitting process. In some cases there may be multiple fits found for
    * a slot.In such cases application fits the matrix with one random word from the list of matches if the fit
    * is not successful it retries with random fit again until successful. This random selection happens only when there
    * are multiple cross matches in a slot.
    *
    * @param matrix input matrix
    * @param size size of the matrix
    * @param inputWords input words to fit
    */
  def runCrossWordFitting(matrix: Array[Array[Char]], size: Int, inputWords: List[String]) = {

    val wordSlots       = getSlots(matrix)
    val candidateWords  = findCandidateWords(wordSlots, inputWords)
    val interSectionPts = findIntersections(wordSlots)

    def solveRecursively(attempt: Int): (Boolean, Array[Array[Char]]) = {
      val wordFits    = findFits(candidateWords, candidateWords, interSectionPts).toMap
      val cloneMatrix = matrix.map(_.clone)
      val status      = fitMatrix(cloneMatrix, wordFits, wordSlots, inputWords)
      if (status == true || attempt > 999)
        (status, cloneMatrix)
      else
        solveRecursively(attempt + 1)
    }
    val matrixSolution = solveRecursively(1)
    val statusMessage  =
      if (matrixSolution._1)
        "SUCCESS"
      else
        "INCOMPLETE/FAILED"
    println(s"Status:$statusMessage Final Output:");
    printMatrix(matrixSolution._2, size)
  }

  def main(args: Array[String]) {
    val SIZE                       = 10
    val matrix: Array[Array[Char]] = Array.ofDim[Char](SIZE, SIZE)
    var line                       = ""
    var i                          = 0
    println("Enter Input Matrix 10*10 (only '+' and '-' allowed")
    while (
      i < 10 && {
        line = StdIn.readLine();
        line != null
      }
    ) {
      val characters = line.toCharArray
      if (characters.length == 10 && (characters forall (x => (x == '-') || (x == '+')))) {
        matrix(i) = line.toCharArray
        i = i + 1
      } else {
        i = 0;
        println("Invalid characters. Please re enter FULL matrix")
      }
    }

    println("Enter Input Words to fit as semicolon(;) separated")
    var correctInput  = true
    var inputWordsStr = ""
    do {
      inputWordsStr = StdIn.readLine()
      if (inputWordsStr.toCharArray forall (x => Character.isLetter(x) || x == ';')) {
        correctInput = true;
        println("OK")
      } else {
        correctInput = false;
        println("Incorrect input - Re enter")
      }
    } while ((!correctInput) && line != null)

    val inputWords = inputWordsStr.split(";").toList

    runCrossWordFitting(matrix, SIZE, inputWords)
  }

  //indicates the direction of wordSlot identified
  sealed trait Direction
  case object HORIZONTAL extends Direction
  case object VERTICAL extends Direction

  //represents each possible wordSlot
  case class WordSlot(slotId: String, slot: List[(Int, Int)], direction: Direction) {
    override def toString: String = s"\nslotId: $slotId slot:$slot direction:$direction"
  }

  //possible words that can fit in a wordSlot
  case class Candidates(slotId: String, direction: Direction, fittableWords: List[String])

  //Intersection Point of crosswords
  case class IntersectionPoint(hslotId: String, vslotId: String, point: (Int, Int), hIndex: Int, vIndex: Int) {
    override def toString: String = s"\n$hslotId|$vslotId @ $point hIndex:$hIndex and vIndex:$vIndex"
  }
}
