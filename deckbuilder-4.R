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


results = data.frame(matrix(nrow = 0, ncol = 5))
colnames(results) = c("win", "cost", "cost2", "cost3", "cost4")

for (j in 1:1000) {
  landCount = sample(1:56, 1)
  lands = data.frame(type = "land", cost = integer(landCount))
  deck = lands
  deck = rbind(deck, data.frame(type="creature", cost=1))
  while (nrow(deck) < 60) {
    tempCost = sample(1:4, 1)
    tempCard = data.frame(type = "creature", cost = tempCost)
    deck = rbind(deck, tempCard)
  }
  tempCost1 = nrow(deck[deck$cost == 1,])
  tempCost2 = nrow(deck[deck$cost == 2,])
  tempCost3 = nrow(deck[deck$cost == 3,])
  tempCost4 = nrow(deck[deck$cost == 4,])
  deck = deck[sample(1:60),]
  hand = deck[1:6,]
  deck = deck[7:60,]
  battlefield = data.frame(matrix(nrow = 0, ncol = 2))
  colnames(battlefield) = c("type", "cost")
  
  opponentsLife = 20
  for (i in 1:5)  {
    if (i == 5) {
      results = rbind(results, data.frame(win = 0, cost1 = tempCost1, cost2 = tempCost2, cost3 = tempCost3, cost4 = tempCost4))
      break
    }
    hand = rbind(hand, deck[1,])
    deck = deck[2:nrow(deck),]
    newBoardState = playTurn(deck, hand, battlefield)
    deck = newBoardState[[1]]
    hand = newBoardState[[2]]
    battlefield = newBoardState[[3]]
    opponentsLife = opponentsLife - sum(battlefield[battlefield$type == "creature", "cost"])
    if (opponentsLife <= 0) {
      results = rbind(results, data.frame(win = 1, cost1 = tempCost1, cost2 = tempCost2, cost3 = tempCost3, cost4 = tempCost4))
      break
    }
  }
  print(paste("Win Count: ", nrow(results[results$win == 1,])))
  print(paste("Loss Count: ", nrow(results[results$win == 0,])))
}