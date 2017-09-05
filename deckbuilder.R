playTurn = function(deck, hand, battlefield, turnsPast) {
  handLands = hand[hand$type=="land",]
  nHandLands = nrow(handLands)
  if (nHandLands > 0) {
    battlefield = rbind(battlefield, handLands[1,])
    if (nHandLands > 1)
      hand = rbind(hand[hand$type=="creature",], handLands[2:nHandLands,])
    else
      hand = hand[hand$type=="creature",]
  }
  nBattlefieldLands = battlefield[battlefield$type=="land"]
  handCreatures = hand[hand$type=="creature",]
  nHandCreatures = nrow(handCreatures)
  handCreatureCosts = hand[hand$type=="creature", "cost"]
  creatureCombos = matrix(0, 2^nHandCreatures, nHandCreatures + 1)
  j = 0
  for (i in 1:nHandCreatures) {
    combinations = combn(handCreatureCosts, i)
    for (k in 1:nrows(combinations)) {
      combo = combinations[,k]
      creatureCombos[j,] = c(combo, integer(nHandCreatures-i), sum(combo))
      j = j + 1
    }
  }
}

landCount <- sample(1:59, 1)
lands <- data.frame(type="land", cost = integer(landCount))
deck <- lands
while (nrow(deck) < 60) {
  tempCost = sample(1:landCount, 1)
  tempCard = data.frame(type = "creature", cost = tempCost)
  deck = rbind(deck, tempCard)
}
deck = deck[sample(1:60),]
startingPlayer = sample(0:1, 1)
turnsPast = 0
if (startingPlayer == 1) {
  hand = deck[1:7,]
  deck = deck[8:60,]
} else {
  hand = deck[1:8,]
  deck = deck[9:60,]
  turnsPast = 1
}
battlefield = data.frame(matrix(ncol = 2, nrow = 0))
colnames(battlefield) <- c("type", "cost")