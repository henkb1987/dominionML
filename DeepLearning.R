init.nn <- function(model.name, dat){
  # clean data
  dat <- dat[complete.cases(dat), ]
  model <- dbn.dnn.train(
    x = dat[, !grepl("b_", colnames(dat))],
    y = dat[, grepl("b_", colnames(dat))],
    hidden = c(250),
    learningrate_scale = .9,
    learningrate = .7,
    numepochs = 1,
    #activationfun = "linear",
    #hidden_dropout = .01,
    output = "sigm"
  )
  save(model, file=model.name)
}
update.nn <- function(model.name, dat){
  # clean data
  dat <- dat[complete.cases(dat), ]
  load(model.name)
  model <- nn.train(
    x = dat[1:100, !grepl("b_", colnames(dat))],
    y = dat[1:100, grepl("b_", colnames(dat))],
    initW = model$W,
    initB = model$B,
    hidden = c(250),
    learningrate_scale = .8,
    learningrate = .7,
    numepochs = 10
  )
  save(model, file=model.name)
}
