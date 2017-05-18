load("card.info")
source("DataFunctions.R")
std.cards <- c("Copper", "Silver", "Gold", "Platina", "Estate", "Duchy", "Province", "Colony")
true.end.decks <- NULL

counter <- 1
while(T){
  if(file.exists(paste0("games_",counter))){
    load(paste0("games_",counter))
    counter <- counter + 1
  }
  Sys.sleep(1)
  cat(".\n")
}



end.points <- NULL
deck.pattern <- ".*?::\\{(.*?)\\}.*?"
points.pattern <- ".*?::\\{\\:.*?\\,.*" # *:44,

end.decks <- matrix(0, length(tmp), 2 * 173)
for(i in 1:length(tmp)){
  if(i%%1e3==0){cat(i,"\n")}
  end.decks[i, ] <- game.to.data.row(i)
}



end.decks <- aaply(.data = 1:length(tmp), .margins = 1, .fun = game.to.data.row)
colnames(end.decks) <- c(paste0("d_",card.info$Plural),paste0("s_", card.info$Singular))

inputs <- end.decks[, grepl("s_",colnames(end.decks))]
inputs[, which(card.info$Singular %in% std.cards)] <- 1
outputs <- end.decks[, grepl("d_",colnames(end.decks))]

for(card in 24:173){
  relevant.kingdoms <- which(inputs[, card] > 0)
  cat(as.character(card.info$Singular[card]),"found in",length(relevant.kingdoms),"kingdoms...\n")
  model <- ranger(
    data = cbind(to.buy = outputs[relevant.kingdoms, card], inputs[relevant.kingdoms, ]),
    dependent.variable.name = "to.buy",
    save.memory = T,
    write.forest = T,
    importance = "impurity",
    min.node.size = 5,
    num.trees = 1e3
  )
  save(model, file=paste0(card.info$Singular[card],"_model_"))
}
