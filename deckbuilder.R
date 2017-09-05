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
  handCreatures = hand[hand$type == "creature",]
  nHandCreatures = nrow(handCreatures)
  handCreatureCosts = hand[hand$type == "creature", "cost"]
  print (handCreatureCosts)
  creatureCombos = matrix(0, nrow = 2^nHandCreatures, ncol = nHandCreatures + 1)
  j = 0
  if (nHandCreatures >= 1) {
    for (i in 1:nHandCreatures) {
      combinations = combn(handCreatureCosts, i)
      for (k in 1:ncol(combinations)) {
        creatureCombo = t(combinations[, k])
        x = tryCatch({
          creatureCombos[j,] = c(creatureCombo, integer(nHandCreatures-i), sum(creatureCombo))
        }, error = function(e) {
          print("hello1")
          print(c(creatureCombo, integer(nHandCreatures-i), sum(creatureCombo)))
        })
        j = j + 1
      }
    }
    validCreatureCombos = creatureCombos[creatureCombos[, (nHandCreatures + 1)] == nBattlefieldLands,]
    if (length(validCreatureCombos) >=1) {
      creatureCombo = tryCatch({
        creatureCombo = validCreatureCombos[1, validCreatureCombos[1,] != 0]
      }, error = function(e) {
        print("hello2")
      }, finally  = {
        creatureCombo = validCreatureCombos[validCreatureCombos[] != 0]
      })
      nCreaturesInCombo = length(creatureCombo)
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

landCount = sample(1:59, 1)
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
while (opponentsLife > 0)  {
  if (lastTurn == TRUE) {
    print ("you lost")
    print ("Battlefield:")
    print (battlefield)
    break
  }
  if (nrow(deck) > 1)
    deck = deck[2:nrow(deck),]
  else
    lastTurn = TRUE
  hand = rbind(hand, deck[1,])
  newBoardState = playTurn(deck, hand, battlefield)
  deck = newBoardState[[1]]
  hand = newBoardState[[2]]
  battlefield = newBoardState[[3]]
  opponentsLife = opponentsLife - sum(battlefield[battlefield$type == "creature", "cost"])
  print(paste("Opponent's Life: ", opponentsLife))
  print(paste("Cards in play: ", nrow(battlefield)))
  print(paste("Cards in deck: ", nrow(deck)))
  print(paste("cards in hand: ", nrow(hand)))
}