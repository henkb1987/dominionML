## predict best buy strategy
load("card.info")
require("ranger")

# make supply
std.cards <- c("Copper", "Silver", "Gold", "Estate", "Duchy", "Province", "Colony")
std.cards <- c("Copper", "Silver", "Gold", "Estate", "Duchy", "Province")
kingdom <- c("Great Hall", "Witch", "Smithy", "Trade Post", "Baron", "Council Room", "Duke", "Steward", "Spy", "Torturer")
#kingdom <- as.character(sample(card.info$Singular[-which(card.info$Singular %in% std.cards)], 10))
# library, moat, thief, adventurer, woodcutter, 'oplicyhter', feast, coppersmith, militia

kingdom.numbers <- which(card.info$Singular %in% std.cards | card.info$Singular %in% kingdom)
input.slices <- rep(0, nrow(card.info))
input.slices[kingdom.numbers] <- 1
input.slices <- data.frame(rbind(input.slices, input.slices))
colnames(input.slices) <- paste0("s_", card.info$Singular)

buy.strat <- data.frame(card = as.character(c(std.cards, kingdom)), n.to.buy = 0, error = 0, rsquared=0, stringsAsFactors = F)
cat("For kingdom",kingdom,":\n")
for(i in 1:nrow(buy.strat)){
  cat("Evaluating",buy.strat$card[i]," ... ")
  load(paste0(buy.strat$card[i],"_model"))
  buy.strat$n.to.buy[i] <- predict(model, input.slices)$predictions[1]
  buy.strat$error[i] <- model$prediction.error
  buy.strat$rsquared[i] <- model$r.squared
  cat("please buy", round(buy.strat$n.to.buy[i],1),"+-",buy.strat$error[i],"\n")
}
