# laad package
library("deepnet")

# laad card info tabelletje en de data
load("card.info")
load("end_decks_135")

# splits outputs en inputs
std.cards <- c("Copper", "Silver", "Gold", "Platinum", "Estate", "Duchy", "Province", "Colony")
inputs <- end.decks[, grepl("s_",colnames(end.decks))]
inputs[, which(card.info$Singular %in% std.cards)] <- 1
outputs <- end.decks[, grepl("d_",colnames(end.decks))]

# deepnet runnen
model <- dbn.dnn.train(
  x = inputs,
  y = outputs,
  hidden = c(175,175),
  learningrate_scale = .9,
  learningrate = .7,
  numepochs = 100,
  #activationfun = "linear",
  #hidden_dropout = .01,
  output = "linear"
)

model.predict <- nn.predict(model, inputs)

# je kan strategyprediction gebruiken om naar predicties te kijken