source("env.R")
source("util.R")

load_predictions = tsv_loader(
    paste(DATA_DIR, "wpmed_predictions.tsv", sep="/"),
    "PREDICTIONS"
)
