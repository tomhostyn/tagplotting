library (dplyr)
library (ggplot2)
library (lubridate)

# define tags
kTags <- c("happy", "sad", "excited", "bored", "cool", "warm", "go", "stay")

# Tag data frame 
# > str(gTags)
# 'data.frame':  9229 obs. of  6 variables:
#   $ id      : num - unique identifier of tag
# $ chain   : num  - unique identifier of chain.  chain = id for original tag
# $ tag     : Factor w/ 8 levels kTag
# $ strength: int  (1-3)
# $ date    : POSIXct,  date & time placed
# $ userID  : num  ID in user pool

# user database
# > str(gUserPool)
# 'data.frame':  10000 obs. of  4 variables:
# $ userID    : int  unique identifier for a user
# $ gender    : Factor w/ 2 levels "Female","Male"
# $ age       : num  32.6 26.6 32.2 22.3 35.8 ...

###############################################################################################################  
#
#  TRIAL SIMULATOR
#
###############################################################################################################  
#  run SimulateTrial ()
#  run saveAll () to save all plots to file



# $ enthusiasm: num  0.0957 0.9771 0.9721 0.5591 0.1929 ...  only relevant for simulator

# currently active users - only for simulator
#gActiveUsers - subset of gUserPool userIDs

# currently inactive users - only for simulator
#gInactiveUsers  - subset of gUserPool userIDs

#define probability of tag occuring in a new post
if (!exists ("kNewProb")){
  kNewProb <- runif (length (kTags))
  kNewProb <- kNewProb/sum(kNewProb)
}

#define probability of tag occuring in a chained post
if (!exists ("kReactProb")){
  kReactProb <- runif (length (kTags))
  kReactProb <- kReactProb/sum(kReactProb)
}

# ratio new tag/total tags
kNewTagRatio <- 1/5

GenerateUsers <- function (num.users) {

  # generate Humans
  
  userID <- 1:num.users
  
  # women are nerds in this universe
  gender <- sample(c("Male", "Female"), num.users, replace=TRUE, prob = c(0.40, 0.60)) 
  
  # pick age using normal distribution around 30, but make sure we have a peak at 20
  age <- abs (c(rnorm (round(num.users * 0.7), mean= 30, sd = 10),
              rnorm (num.users - round(num.users * 0.7), mean= 20, sd = 3)))
  
  enthusiasm <- runif (num.users)
  
  # females are a bit more enthusiastic
  enthusiasm [gender == "Female"] <- enthusiasm [gender == "Female"] * 1.2 

  # 30 somethings are a lot more enthusiastic
  enthusiasm [30 <= age && age < 40] <- enthusiasm [30 <= age && age < 40] * 1.5 
  
  data.frame (userID, gender, age, enthusiasm)
}

GenerateNewTag <- function(date, user) {
  newRow <- data.frame (id = gTagCount,
                        chain=gTagCount, 
                        tag = sample (kTags, 1, prob=kNewProb),
                        strength = sample(1:3, 1),
                        date = date,
                        userID = user)
  gTagCount <<- gTagCount + 1
  gTags <<- rbind (gTags, newRow)
}

GenerateReactionTag <- function(date, user) {
  # TODO : don't pick uniformly!
  prevEmo <- sample (1:nrow(gTags[gTags$chain == gTags$id, ]), 1)
  newRow <- data.frame (id = gTagCount,
              chain=gTags[gTags$chain == gTags$id, ][prevEmo, "id"], 
              tag = sample (kTags, 1, prob=kReactProb),
              strength = sample(1:3, 1),
              date = date,
              userID = user)
  gTagCount <<- gTagCount + 1
  gTags <<- rbind (gTags, newRow)
}

Recruit <- function (N){
  recruited <- sample (gInactiveUsers, min(length(gInactiveUsers), N), replace=FALSE)
  gInactiveUsers <<- gInactiveUsers[! gInactiveUsers %in% recruited]
  gActiveUsers <<- c(gActiveUsers, recruited)
}

Dropoff <- function (N){
  dropoff <- sample (gActiveUsers, min(length(gActiveUsers), N), replace=FALSE)
  gActiveUsers <<- gActiveUsers[! gActiveUsers %in% dropoff]
  gInactiveUsers <<- c(gInactiveUsers, dropoff)
}

GetTodaysUsers <- function (factor) {
  todaysEnthusiasm <- gUserPool[gActiveUsers, "enthusiasm"] * abs(rnorm (gActiveUsers, mean=1)) * factor
  numTags <- sum (todaysEnthusiasm)
  sample (gActiveUsers, round(numTags), replace = TRUE, prob = todaysEnthusiasm/numtags)
}


SimulateDay <- function (date){

  #
  #  Users come and go
  #
  
  # have 1 recruitment day per month on avg
  recruitmentDay <- sample (c(1,0), 1, prob = c(1/30, 29/30))
  
  # recruit about 20 users on a recruitment day
  recruited <- recruitmentDay * rnorm (1, mean=20, sd=2)
  Recruit (recruited)
  
  # some dropoff. say ~ 0.03% per day 
  dropoff <- round(abs(rnorm (1, mean=length(gActiveUsers) * 0.003, sd=3)))
  Dropoff(dropoff)

  
  # viral effect : some users are added without recruitment.  say ~0.05% of active user base
  recruited <- round(abs(rnorm (1, mean=length(gActiveUsers) * 0.005, sd=3)))
  Recruit (recruited)
  
  
  #
  # users create tags
  #
  todaysEnthusiasm <- gUserPool[gActiveUsers, "enthusiasm"] * abs(rnorm (gActiveUsers, mean=1))
  numTags <- sum (todaysEnthusiasm)
  users <- sample (gActiveUsers, round(numTags), replace = TRUE, prob = todaysEnthusiasm/numTags)
  
  split <- round (length(users)*kNewTagRatio)
  sapply (1:split, 
         function (u) {GenerateNewTag(date, u)})
  
  sapply (split+1:length(users), 
         function (u) {GenerateReactionTag(date, u)})
}

SimulateTrial <- function () {
  gUserPool <<- GenerateUsers (10000)
  gActiveUsers <<- c()
  gInactiveUsers <<- 1:nrow(gUserPool)

  gTags <<- data.frame()
  gTagCount <<- 1
  
  # start with 20 beta users
  Recruit (20)
  
  # run for some time
  start <- ymd ("20141215")
  test.period <- start + days(0:99)
  
  sapply (test.period, SimulateDay)
  gTags
}

###############################################################################################################  
#
#  Plot the data !
#
###############################################################################################################  

kOutputDir <- "tagproject/"

saveggplot <- function (plot_f){
  filename <- paste (kOutputDir, as.character(substitute(plot_f)), ".png", sep="")
  p <- plot_f()
  ggsave(filename=filename, plot=p, dpi=100)
}

saveplot <- function (plot_f){
  filename <- paste (kOutputDir, as.character(substitute(plot_f)), ".png", sep="")
  png(file=filename, width=480,height=480)
  plot_f()  
  dev.off()
}

saveAll <- function () {
  output <- file.path(getwd(), kOutputDir)
  if (!file.exists(output)){
    dir.create(output)
  }
  
  saveggplot(plotUsers1)
  saveggplot(plotUsers2)
  saveggplot(plotUsers3)
  saveggplot(plotUsers4)
  saveggplot(plotUsers5)
  saveplot(plotUsers6)
  saveplot(plotUsers6b)
  saveplot(plotUsers7)
  saveggplot(plotChains1) 
  saveggplot(plotChains2)
  saveplot(plotChains3)
  saveplot(plotChains4)
  
}


plotUsers1 <- function () {
  #
  #  plot Weekly active users
  #  using ggplot
  start <- min(gTags$date)
  
  weeklyUse  <- gTags %>%
    transform (week = date - wday(date)) %>%
    group_by(week) %>% 
    summarize (sum = length(unique (userID)))
  
  ggplot (weeklyUse, aes(x=week, y=sum)) + 
    geom_point() + geom_line() + geom_smooth(method="lm") +
    labs (x = "Week", y = "Unique users", title="Unique weekly users")
}

plotUsers2 <- function () {
  #
  #  plot Weekly created tags per gender
  #  using ggplot
  start <- min(gTags$date)
  
  by.gender  <- gTags %>%
    inner_join(gUserPool, by="userID") %>%
    transform (week = date - wday(date)) %>%
    group_by(week, gender) %>% 
    summarize (sum = length(unique (chain)))
    
  ggplot (by.gender, aes(x=week, y=sum)) + 
    facet_wrap(~gender) +
    geom_point() + geom_line() + geom_smooth(method="lm") +
    labs (x = "Week", y = "created tags", title="Created tags over time (per Gender)")
}

plotUsers3 <- function () {
  #
  #  plot Weekly created tags per age group
  #  using ggplot
  start <- min(gTags$date)
  
  by.age  <- gTags %>%
    inner_join(gUserPool, by="userID") %>%
    transform (week = date - wday(date)) %>%
    transform (age.group = floor (age/10) * 10 ) %>%
    group_by(week, age.group) %>% 
    summarize (sum = length(unique (chain)))
  
  ggplot (by.age, aes(x=week, y=sum)) + 
    facet_wrap(~age.group) +
    geom_point() + geom_line() + geom_smooth(method="lm") +
    labs (x = "Week", y = "Created tags", title="Created tags over time (per age group)")
}

plotUsers4 <- function () {
  #
  #  plot created tags per age group & gender
  #  using ggplot
  start <- min(gTags$date)
  
  by.age  <- gTags %>%
    inner_join(gUserPool, by="userID") %>%
    group_by(age, gender) 
  
  ggplot (by.age, aes(x=age)) + 
    facet_wrap(~gender) +
    geom_histogram(binwidth=1, colour="black", fill="white")+
    labs (x = "Age", y = "Created tags", title="Created tags by age")
}

plotUsers5 <- function () {
  #
  #  plot created tags per age group & gender (relative)
  #  using ggplot
    
  by.age  <- gTags %>%
    inner_join(gUserPool, by="userID") %>%
    transform (age = floor(age/5) * 5) %>%
    group_by(gender, age)  %>% 
    summarize (sum = length(id)/length(unique(userID)))
  
  ggplot (by.age, aes(x=age, y = sum)) + 
    facet_wrap(~gender) +
    geom_point() +
    labs (x = "Age", y = "Created tags", title="Created tags by age (relative)")
}

plotUsers6 <- function () {
  #
  # see if we can discover clusters in user enthusiasm as judged by tagging behaviour
  #
  
  by.users <- gTags %>%
    group_by(userID)  %>% 
    summarize (sum = length(id)) %>%
    inner_join(gUserPool, by="userID") %>%
    transform(gender = (as.numeric(gender)-1)*50) %>%
    select (sum, gender, age)
  
  dataMatrixOrdered <- (as.matrix (by.users))
  
  #  svd1 <- svd(scale(dataMatrixOrdered))  scaling does not make sense across columns ?
  svd1 <- svd((dataMatrixOrdered))
  par(mfrow = c(1, 3))
  image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])
  plot(svd1$u[, 1], , xlab = "Row", ylab = "First left singular vector", 
       pch = 19)
  plot(svd1$v[, 1], xlab = "Column", ylab = "First right singular vector", pch = 19)
}

plotUsers6b <- function () {
  #
  # see if we can discover clusters in user enthusiasm as judged by tagging behaviour
  #
  
  by.users <- gTags %>%
    group_by(userID)  %>% 
    summarize (sum = length(id)) %>%
    inner_join(gUserPool, by="userID") %>%
    transform(gender = scale(as.numeric(gender)-1),
              sum = scale (sum),
              age = scale(age)) %>%
    select (sum, gender, age)
  
  dataMatrixOrdered <- (as.matrix (by.users))
  
  #  svd1 <- svd(scale(dataMatrixOrdered))  scaling does not make sense across columns ?
  svd1 <- svd((dataMatrixOrdered))
  par(mfrow = c(1, 3))
  image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])
  plot(svd1$u[, 1], , xlab = "Row", ylab = "First left singular vector", 
       pch = 19)
  plot(svd1$v[, 1], xlab = "Column", ylab = "First right singular vector", pch = 19)
}


plotUsers7 <- function () {
  #
  # see if we can discover clusters in user enthusiasm as judged by tagging behaviour
  # not sure how to interpret these correctly.
  
  by.users <- gTags %>%
    group_by(userID)  %>% 
    summarize (sum = length(id)) %>%
    inner_join(gUserPool, by="userID") %>%
    transform(gender = (as.numeric(gender)-1)*50) %>%
    select (sum, gender, age)
  
  dataMatrixOrdered <- (as.matrix (by.users))
  heatmap(dataMatrixOrdered, 
          main="clear patterns - how to interpret ?")  
}


plotChains1 <- function () {
  #
  #  plot emotion tags length
  # 
    
  chain  <- gTags %>%
    group_by(chain) %>% 
    summarize (sum = length(id)-1)

  histo <- chain %>%
    group_by (sum) %>%
    summarize (hist = length(chain))
    
  ggplot (chain, aes(x=sum)) + 
    geom_histogram (binwidth=1, colour="black", fill="white") + 
    geom_vline(aes(xintercept=mean(sum)),
               color="red", linetype="dashed", size=1) +
    annotate (geom="text", x=mean(chain$sum), y = 0.8 * max(histo$hist), hjust=-0.2, 
              label=paste ("mean =", round (mean(chain$sum)), 2)) + 
    annotate (geom="text", x=max(chain$sum), y = 0.1 * max(histo$hist), hjust= 1, 
              label=paste ("max =", max(chain$sum))) + 
    labs (x = "Number of chained tags", y = "Count", title="Histogram of tag chains")
}

plotChains2 <- function () {
  #
  #  plot emotion tags length
  # 
  cbPalette <- c("happy"="#999999", "sad"="#E69F00", "excited"="#56B4E9",
                "bored"="#009E73", "cool"="#F0E442", "warm"="#0072B2", "go"="#D55E00", "stay"="#CC79A7")

  
  chain  <- gTags %>%
    transform(origTag = gTags[chain, "tag"]) %>%
    filter (tag != chain) %>%  # count only the chained tags
    group_by(origTag, tag) %>% 
    summarize (sum = length(id))

  ggplot (chain, aes(x=tag, y=sum, colour=tag)) + 
    facet_wrap(~origTag) +
    geom_point() +
    scale_color_manual (values=cbPalette) +
    theme(axis.text.x = element_text(angle = 90)) +
    labs (x = "Response", y = "Count", title="Responses per original tag")
}

plotChains3 <- function () {
  #
  #  plot emotion tags length over time as heatmap.
  # 
  chain  <- gTags %>%
    group_by(chain) %>% 
    summarize (sum = length(id)-1)
  
  width = 10
  padding <- (width - length(chain$sum) %% width) %% width
  
  plot <- c(chain$sum, rep(0, padding))
  m <- matrix (rev(plot), ncol=width , byrow=TRUE)
  heatmap(m, 
          Rowv=NA, Colv=NA, 
          scale="none",
          main="Heatmap of emotion chain length over time")
}

plotChains4 <- function () {
  #
  #  plot emotion tags length over time as heatmap.
  # 
  chain  <- gTags %>%
    group_by(chain) %>% 
    summarize (sum = length(id)-1)
  
  width = 10
  padding <- (width - length(chain$sum) %% width) %% width
  
  plot <- c(chain$sum, rep(0, padding))
  m <- matrix (rev(plot), ncol=width , byrow=TRUE)
  image(t((m)))
}
