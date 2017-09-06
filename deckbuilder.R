playTurn = function(deck, hand, battlefield) {
  handLands = hand[hand$type == "land",]
  nHandLands = nrow(handLands)
  if (nHandLands > 0) {
    battlefield = rbind(battlefield, handLands[1,])
    if (nHandLands > 1)
      hand = rbind(hand[hand$type == "creature",], handLands[2:nHandLands,])
    else
      hand = hand[hand$type == "creature",]
  }
  nBattlefieldLands = nrow(battlefield[battlefield$type == "land",])
  handCreatures = hand[hand$type == "creature",, drop=FALSE]
  nHandCreatures = nrow(handCreatures)
  creatureCombos = matrix(0, nrow = 2^nHandCreatures, ncol = nHandCreatures + 1)
  j = 1
  if (nHandCreatures >= 1) {
    for (i in 1:nHandCreatures) {
      if (nHandCreatures == 1)
        combinations = matrix(handCreatures$cost)
      else
        combinations = combn(handCreatures$cost, i)
      for (k in 1:ncol(combinations)) {
        creatureCombo = t(combinations[, k])
        creatureCombos[j,] = c(creatureCombo, integer(nHandCreatures-i), sum(creatureCombo))
        j = j + 1
      }
    }
    validCreatureCombos = creatureCombos[creatureCombos[, (nHandCreatures + 1)] <= nBattlefieldLands,, drop=FALSE]
    validCreatureCombos = validCreatureCombos[validCreatureCombos[, (nHandCreatures + 1)] > 0,, drop=FALSE]
    validCreatureCombos = validCreatureCombos[order(-validCreatureCombos[, nHandCreatures + 1]),,drop=FALSE]
    if (nrow(validCreatureCombos) >= 1) {
      creatureCombo = validCreatureCombos[1, validCreatureCombos[1,] != 0, drop=FALSE]
      nCreaturesInCombo = ncol(creatureCombo)
      for (i in 1:nCreaturesInCombo) {
        if (i < nCreaturesInCombo) {
          creatureCost = creatureCombo[i]
          creatures = hand[hand$cost == creatureCost,]
          battlefield = rbind(battlefield, creatures[1,])
          nCreatures = nrow(creatures)
          if (nCreatures > 1)
            hand = rbind(hand[hand$cost != creatureCost,], creatures[2:nCreatures,])
          else
            hand = hand[hand$cost != creatureCost,]
        }
      }
    }
  }
  return (list(deck, hand, battlefield))
}

landCount = sample(1:54, 1)
lands = data.frame(type = "land", cost = integer(landCount))
deck = lands
while (nrow(deck) < 60) {
  tempCost = sample(1:landCount, 1)
  tempCard = data.frame(type = "creature", cost = tempCost)
  deck = rbind(deck, tempCard)
}
deck = deck[sample(1:60),]
hand = deck[1:6,]
deck = deck[7:60,]
battlefield = data.frame(matrix(nrow = 0, ncol = 2))
colnames(battlefield) = c("type", "cost")

opponentsLife = 20
lastTurn = FALSE
turnsPast = 0
while (opponentsLife > 0)  {
  if (lastTurn == TRUE) {
    print(paste("Opponent's Life: ", opponentsLife))
    print(paste("Cards in play: ", nrow(battlefield)))
    print(paste("Cards in deck: ", nrow(deck)))
    print(paste("cards in hand: ", nrow(hand)))
    print ("You lost!")
    break
  }
  turnsPast = turnsPast + 1
  hand = rbind(hand, deck[1,])
  if (nrow(deck) == 1) {
    lastTurn = TRUE
    deck = data.frame(matrix(nrow = 0, ncol = 2))
    colnames(deck) = c("type", "cost")
  } else {
    deck = deck[2:nrow(deck),]
  }
  newBoardState = playTurn(deck, hand, battlefield)
  deck = newBoardState[[1]]
  hand = newBoardState[[2]]
  battlefield = newBoardState[[3]]
  opponentsLife = opponentsLife - sum(battlefield[battlefield$type == "creature", "cost"])
  print(paste("Opponent's Life: ", opponentsLife))
  print(paste("Cards in play: ", nrow(battlefield)))
  print(paste("Cards in deck: ", nrow(deck)))
  print(paste("cards in hand: ", nrow(hand)))
  if (opponentsLife <= 0)
    print(paste("You won in ", turnsPast))
}