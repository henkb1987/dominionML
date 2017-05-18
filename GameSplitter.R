#f <- "counsil.xz" # dit is het JSON bestand in de working directory
#con <- file(description = f, open = "r")

counter <- 103
batch.size <- 1e5

while(T){
  start.time <- proc.time()[3]
  cat("Scanning ")
  tmp <- scan(
    quiet = F,
    file = "counsil.xz", 
    what = "character",
    skip = batch.size * (counter - 1),
    nlines = batch.size,
    multi.line = T
  )
  cat("Concatenating ")
  tmp <- paste(tmp, collapse = "")
  cat("Splitting ")
  tmp <- strsplit(tmp,"\\{_id:")[[1]][2:(batch.size + 1)]
  cat("Writing",paste0("games_",counter,"\n",collapse = ""))
  save(tmp, file=paste0("games_",counter,collapse = ""))
  cat("Ready. Took",round(proc.time()[3] - start.time),"seconds\n")
  counter <- counter + 1
  gc()
}
