library(plumber)

pr <- plumb("https://raw.githubusercontent.com/pjournal/boun01g-hisrustu/gh-pages/Plumber%20Source%20Code/plumber.R")

pr$run(port=8000)

