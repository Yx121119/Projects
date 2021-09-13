# Conclude functions that used to simplify the codes in the project
library(patchwork)
library(mixtools)
library(ggplot2)
library(car)

# function: dplot()
#  Draw the density plot for given continent
#  INPUT:
#   Data: raw data set
#   type: country type
#  OUTPUT:
#   density plot for fertility of specific continent

dplot <- function(Data, type) {
  conType <- c("Americas", "Africa", "Asia", "Europe", "Oceania")
  # Check if the given continent type is contained in the data set
  if (type %in% conType) {
    sData <- Data[Data$continent == type, ]
    ggplot(sData, aes(fertility)) + geom_density(color = "black",
                                                 fill = "gray") +
      ggtitle(type) + theme(plot.title = element_text(hjust = 0.5))
  } else{
    stop("Error: country types are: Americas, Africa, Asia, Europe, Oceania")
  }
}

# function: allPlot()
# Plot the Density plots for total response and all five continents
#  Input: Data 
#  Output: Density plots for total response and all five continents

allPlot <- function(data) {
  p <- ggplot(data, aes(fertility)) + geom_density(color = "grey", fill = "steelblue") +
    ggtitle("Total") +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Density plot for each continent
  # Africa
  p1 <- dplot(data, "Africa")
  
  # Americans
  p2 <- dplot(data, "Americas")
  
  # Aisa
  p3 <- dplot(data, "Asia")
  
  # Europe
  p4 <- dplot(data, "Europe")
  
  # Ocenia
  p5 <- dplot(data, "Oceania")
  return(p + p1 + p2 + p3 + p4 + p5)
}


# function: subData()
#  Substract dataset from the raw data set
#  INPUT:
#    rawData: The original data set
#    str: substract condition eg. Continent type
#    colName: the column(s) to be substract
#  OUTPUT: 
#    data frame for the substrat data

subData <-
  function(rawData, colName, condition, outCol = "fertility") {
    conType <- c("Americas", "Africa", "Asia", "Europe", "Oceania")
    # check if the given information is contained in the data set
    if (condition %in% conType
        && colName %in% colnames(rawData)) {
      sdata <- rawData[rawData[colName] == condition, ]
    } else{
      stop("Error:No columns found in the data set")
    }
    return(sdata[, outCol])
  }


# function: init()
#   Generate initial values for distributions under the assumption of Gaussian
#   Mixture distribution
#   INPUT:
#     Data: corresponding fertility data used for initializing
#     Type: which continent the data belongs to
#   Output:
#     list of initial values including Means:mean, std:standard deviation, lambda

init <- function(Data, Type) {
  conType <- c("Americas", "Africa", "Asia", "Europe", "Oceania")
  # Check if the given information is contained in the data set
  if (Type %in% conType) {
    # Initialize the estimates
    if (Type == "Asia") {
      Means <- c(2.5, 6.25)
    } else if (Type == "Africa") {
      Means <- c(2.5, 6.5)
    } else if (Type == "Americas") {
      Means <- c(2.5, 6)
    } else if (Type == "Europe") {
      Means <- c(1.5, 2)
    } else{
      Means <- c(2.1, 4.5, 6.5)
    }
  }
  std <- rep(1, length(Means))
  lambda <- rep(1 / length(Means), length(Means))
  return(list(
    "Means" = Means,
    "std" = std,
    "lambda" = lambda
  ))
}

# function: paramEstimate()
#  Estimate the parameters for the fertility distribution of each continent
#  INPUT:
#   data: fertility data for corresponding continent
#   normAssumption: with or without the assumption on normality. Default: TRUE
#   conType: which continent to be estimated
#  OUTPUT:
#   with normality assumption Return list of mean and standard deviation
#   without normality assumption, assume the distribution follows a Gaussian
#   Mixture distribution. Return a list of parameters of each components
paramEstimate <- function(data,
                          conType = c("Americas", "Africa", "Asia",
                                      "Europe", "Oceania"),
                          Assumption = TRUE) {
  parameters <- c()
  nonparam <- list()
  for (i in 1:length(conType)) {
    # subtract corresponding data set
    sdata <-
      subData(data, colName = "continent", condition = conType[i])
    if (Assumption) {
      # Under normal assumption, calculate mean and sigma
      parameters <- c(parameters, mean(sdata), sd(sdata))
    } else{
      # Without normal assumption, apply GMM to estimate parameters
      inits <- init(sdata, Type = conType[i])
      mixmod <- normalmixEM(
        sdata,
        lambda = inits$lambda,
        mu = inits$Means,
        sigma = inits$std,
        epsilon = 10 ** (-8)
      )
      nonparam[[conType[i]]]$mu <- mixmod$mu
      nonparam[[conType[i]]]$sigma <- mixmod$sigma
      nonparam[[conType[i]]]$lambda <- mixmod$lambda
    }
  }
  if (Assumption) {
    param <- matrix(parameters, ncol = length(conType), nrow = 2)
    rownames(param) <- c("mu", "sigma")
    colnames(param) <- conType
    return(data.frame(param))
  } else{
    return(nonparam)
  }
}

# function Gmgenetor()
# Generate random numbers follow a Gaussian mixture distribution
#  Input:
#   lambda: lambdas for the distribution
#   mu: mus for the distribution
#   sd: sigmas for the distribution
#   n: number of random numbers to be generated
#  conType: continent type
#  Output: n random numbers follow a Gaussian mixture distribution


Gmgenerator <- function(lambda, mu, sd, n, conType) {
  # Generate n random numbers follow a uniform distribution
  u <- runif(n)
  Grv <- rep(0, n)
  for (i in 1:n) {
    # Consider the special case when continent is "Oceania"
    if (conType == "Oceania") {
      # Generate random numbers by composition approach
      if (u[i] < lambda[1]) {
        Grv[i] = rnorm(1, mu[1], sd[1])
      } else if (u[i] < lambda[1] + lambda[2]) {
        Grv[i] = rnorm(1, mu[2], sd[2])
      } else{
        Grv[i] = rnorm(1, mu[3], sd[3])
      }
    } else{
      # Generate random numbers by composition approach
      if (u[i] < lambda[1]) {
        Grv[i] = rnorm(1, mu[1], sd[1])
      } else{
        Grv[i] = rnorm(1, mu[2], sd[2])
      }
    }
  }
  return(Grv)
}



# function: simGenerator()
#   Simulate corresponding distribution and return simulated values
#   INPUT:
#    data: fertility data for corresponding continent
#    params: parameters under corresponding assumptions
#    Assumption: with or without the assumption on normality. Default: TRUE
#    conType: which continent to be estimated
#    n: the number of simulated data. Default: the number of input data
# Note: If Assumption = FALSE, params should be the list of parameters for 
#       Gaussian mixture distributions for the five continents.
#       If Assumption = TRUE, params should be the data frame of parameters for 
#       normal distributions for the five continents.

simGenerator <- function(data,
                         params,
                         conType,
                         n = 1000,
                         Assumption = TRUE) {
  simData <- c()
  Type <- c("Americas", "Africa", "Asia", "Europe", "Oceania")
  sdata <-
    subData(data, colName = "continent", conType, outCol = "fertility")
  if (conType %in% Type) {
    if (Assumption) {
      # Generate data from normal distribution
      simData <-
        rnorm(n, params[conType][1, ], params[conType][2, ])
    } else{
      # Generate data from Gaussian mixture distribution
      simData <-
        Gmgenerator(params[[conType]]$lambda, params[[conType]]$mu,
                    params[[conType]]$sigma, n, conType = conType)
    }
    return(simData)
  } else{
    # Output error information
    stop("Error: Given counrty types are Americas",
         "Africa",
         "Asia",
         "Europe",
         "Oceania")
  }
}

# function: newData()
# Generating new data frame for given information
#   Input:
#    data: raw data
#    params: parameters under corresponding assumptions
#    Assumption: with or without normal assumption
#    ConType: specific continent type
#    Type: labels used to category the new data. 
#          Default:"Americas", "Africa", "Asia", "Europe", "Oceania"
#    samplesize: sample size for each category. 
#                Default:1546, 2203, 1598, 1418, 374.
# Note: If Assumption = FALSE, params should be the list of parameters for 
#       Gaussian mixture distributions for the five continents.
#       If Assumption = TRUE, params should be the data frame of parameters for 
#       normal distributions for the five continents.

newData <- function(data,
                    params,
                    samplesize = c(1546, 2203, 1598, 1418, 374),
                    conType,
                    Assumption,
                    Type = c("Americas", "Africa", "Asia", "Europe", "Oceania"))
{
  simD <- c()
  newcat <- c()
  for (i in 1:length(samplesize)) {
    Grv <- simGenerator(
      data,
      params = params,
      conType = conType[i],
      Assumption = Assumption,
      n = samplesize[i]
    )
    simD <- c(simD, Grv)
    newcat <- c(newcat, rep(Type[i], length(Grv)))
  }
  newD <- data.frame(simD, newcat)
  colnames(newD) <- c("fertility", "continent")
  return(newD)
}



# function: testValue()
#  Calculate the pvalue on given data set.If pValue is larger than a given
#  alpha, set the indicator to be 1.Otherwise indicator = 0. 
#  Output the value of indicator.
#  INPUT:
#   data: the simulated data to be tested
#   alpha: significant level. Default = 0.05
#   f: method type: "Anova" / "Kruskall-Wallis"
#  OUTPUT:
#   indicator: 1 / 0.
testValue <- function(data,
                      alpha = 0.05,
                      f = "Anova") {
  indicator = 0
  # Check the given information
  if (!(f %in% c("Anova", "Kruskall-Wallis"))) {
    stop("Input Error: Input can be: Anova or Kruskall-Wallis")
  } else if (f == "Anova") { # Parametric method
    fit <- lm(fertility ~ as.factor(continent), data = data)
    test <- anova(fit)
    if (test$`Pr`[1] <= alpha) {
      indicator = 1
    }
  } else{# Non-parametric method
    test <- kruskal.test(fertility ~ continent, data = data)
    if (test$p.value <= alpha) {
      indicator = 1
    }
  }
  return(indicator)
}




# function: testOn()
# Calculate the value of size or power by Monte Carlo simulation method.
# Return a list for simulated value for both parametric 
# and non-parametric method.
#   Input:
#    data: raw data
#    estimates: parameters under corresponding assumptions
#    Assumption: with or without normal assumption
#    ConType: specific continent type
#    Type: labels used to category the new data. 
#          Default:"Americas", "Africa", "Asia", "Europe", "Oceania"
#    samplesize: sample size for each category. 
#    n: number of interactions. Default: 1000
#    alpha: significant level. Default = 0.05
#    stats: which value to be calculated:"size"/"power"
#   Output: list of simulated value for both parametric 
# and non-parametric method.
testOn <-
  function(data,
           estimates,
           conType,
           samplesize,
           n = 1000,
           stats = "size",
           Assumption = TRUE,
           alpha = 0.05,
           Type = c("Americas", "Africa", "Asia", "Europe", "Oceania")) {
    resParam <- c()
    resNon <- c()
    for (i in 1:n) {
      # Generating new data set under given information
      test <- newData(
        data,
        params = estimates,
        conType = conType,
        Assumption = Assumption,
        samplesize = samplesize,
        Type = Type
      )
      # Calculate size or power
      resParam[i] <- testValue(test, f = "Anova", alpha = alpha)
      resNon[i] <- testValue(test, f = "Kruskall-Wallis", alpha = alpha)
    }
    mparam = mean(resParam)
    mnon = mean(resNon)
    # Return corresponding results
    if (stats == "size") {
      return(list("Param-size" = mparam, "Nonparam-size" = mnon))
    } else{
      return(list("Param-power" = mparam, "Nonparam-power" = mnon))
    }
  }
