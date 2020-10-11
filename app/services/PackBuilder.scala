package services

import models._

trait PackBuilder {
  def buildPack(round: Int): CardSet
}
