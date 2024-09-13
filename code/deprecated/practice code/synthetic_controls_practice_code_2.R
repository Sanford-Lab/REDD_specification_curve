# Pracice Danilo Freire Example

# install packages
install.packages("Synth", repos = "http://cran.us.r-project.org")
install.packages("gsynth", repos = "http://cran.us.r-project.org")

library("Synth")
library ("gsynth")

# simulating data
set.seed(1)
year <- rep(1:30, 10) 
state <- rep(LETTERS[1:10], each = 30)

# here you can think of X1, X2, as two randomly generated state-related variables, like population and percentage of whites
X1 <- round(rnorm(300, mean = 2, sd = 1), 2)
X2 <- round(rbinom(300, 1, 0.5) + rnorm(300), 2)

# y is the the dependent variable
Y <- round(1 + 2*X1 + rnorm(300), 2)

# combines all above data into one data frame
df <- as.data.frame(cbind(Y, X1, X2, state, year))

# converts specific columns to their approriate data types
df$Y <- as.numeric(as.character(df$Y))
df$X1 <- as.numeric(as.character(df$X1))
df$X2 <- as.numeric(as.character(df$X2))
df$year <- as.numeric(as.character(df$year))
df$state.num <- as.numeric(df$state)
df$state <- as.character(df$state)

# more data organization
df$T <- ifelse(df$state == "A" & df$year >= 15, 1, 0)
df$Y <- ifelse(df$state == "A" & df$year >= 15, df$Y + 20, df$Y)

# looking at the data set
str(df)

head(df)

# now we estimate the models
# x1, in agrocortex, would be something like percentage of forest cover. y would be amount of deforestation?
dataprep.out <-
  dataprep(df,
           predictors = c("X1", "X2"),
           dependent     = "Y",
           unit.variable = "state.num",
           time.variable = "year",
           unit.names.variable = "state",
           treatment.identifier  = 1,
           controls.identifier   = c(2:10),
           time.predictors.prior = c(1:14),
           time.optimize.ssr     = c(1:14),
           time.plot             = c(1:30)
  )

# run synth
synth.out <- synth(dataprep.out)

print(synth.tables   <- synth.tab(
  dataprep.res = dataprep.out,
  synth.res    = synth.out)
)
