#----------------------------------------0----------------------------------------#
# Load the libraries
library("quantmod")
library("rjson")
library(data.table)
library(priceR)
library("ggplot2")



#----------------------------------------1----------------------------------------#
# Plot the TWTR USA graph
# Get the data, in the date range
twtr.p_NY <- getSymbols("TWTR", env=NULL, from = "2021-12-01", to = "2022-04-30", auto.assign = TRUE)

# Plot the graph
plot(weeklyReturn(twtr.p_NY, subset="2021-12-01::2022-04-30"), main="Weekly return of TWTR", col ="blue")



#----------------------------------------2----------------------------------------#
# Find the peak
# List of all weekly returns
weeklyReturn(twtr.p_NY, subset="2021-12-01::2022-04-30")
# Peak
max(weeklyReturn(twtr.p_NY, subset="2021-12-01::2022-04-30"))



#----------------------------------------3----------------------------------------#
# Plot the candle chart
candleChart(twtr.p_NY, subset="2021-12-01::2022-04-30", name="TWTR", theme ="white", up.col = "black", dn.col = "white")
# Plot the chart series
chartSeries(twtr.p_NY, subset="2021-12-01::2022-04-30", TA = NULL, type="line", name="TWTR", theme="white")
# add the Bollinger Bands to the current chart, moving average = 10, standard deviation = 2
addBBands(n=20, sd=2, draw = 'bands')
# add the Relative Strength Index indicator to the chart
addRSI(n=30)
#chartSeries(RSI(twtr.p_NY$TWTR.Close, n=30), theme = "white", TA = NULL)  #plot of the RSI chart alone
#chartSeries(MACD(twtr.p_NY$TWTR.Close), theme = "white", TA = NULL)  #plot of the MACD chart alone
addMACD()



#----------------------------------------4----------------------------------------#
getSupport_points <- function(twtr_closures){
  twtr_closures <- twtr.p_NY$TWTR.Close
  index <- spacing <- 3
  support_dates <- list()
  support_values <- list()

  for(index in seq(from = spacing, to = length(twtr_closures)-spacing, by = 1)) {
    if(twtr_closures[[index-2]] > twtr_closures[[index-1]]) {
      if(twtr_closures[[index-1]] > twtr_closures[[index]]) {
        if(twtr_closures[[index]] < twtr_closures[[index+1]]) {
          if(twtr_closures[[index+1]] < twtr_closures[[index+2]]) {
            current_index <- length(support_dates)+1
            support_dates[current_index] <- format(index(twtr_closures[index]), "%Y-%m-%d")
            support_values[current_index] <- twtr_closures[index]
          }
        }
      }
    }
  }
  support_points <- cbind(support_dates, support_values)
  return(support_points)
}

getResistance_points <- function(twtr_closures){
  twtr_closures <- twtr.p_NY$TWTR.Close
  index <- spacing <- 3
  resistance_dates <- list()
  resistance_values <- list()

  for(index in seq(from = spacing, to = length(twtr_closures)-spacing, by = 1)) {
    if(twtr_closures[[index-2]] < twtr_closures[[index-1]]) {
      if(twtr_closures[[index-1]] < twtr_closures[[index]]) {
        if(twtr_closures[[index]] > twtr_closures[[index+1]]) {
          if(twtr_closures[[index+1]] > twtr_closures[[index+2]]) {
            current_index <- length(resistance_dates)+1
            resistance_dates[current_index] <- format(index(twtr_closures[index]), "%Y-%m-%d")
            resistance_values[current_index] <- twtr_closures[index]
          }
        }
      }
    }
  }
  resistance_points <- cbind(resistance_dates, resistance_values)
  return(resistance_points)
}


support_list <- getSupport_points(twtr.p_NY[,"TWTR.Close"])
support_list <- support_list[-c(1, 2, 3, 4),]

resistance_list <- getResistance_points(twtr.p_NY[,"TWTR.Close"])
resistance_list <- resistance_list[-c(1, 2, 3, 4, 5, 8, 9),]


support_x1 <- as.Date(support_list[[1,1]], "%Y-%m-%d")
support_x2 <- as.Date(support_list[[2,1]], "%Y-%m-%d")
x_support <- c(support_x1, support_x2)
support_y1 <- support_list[[1,2]]
support_y2 <- support_list[[2,2]]
y_support <- c(support_y1, support_y2)

resistance_x1 <- as.Date(resistance_list[[1,1]], "%Y-%m-%d")
resistance_x2 <- as.Date(resistance_list[[2,1]], "%Y-%m-%d")
x_resistance <- c(resistance_x1, resistance_x2)
resistance_y1 <- resistance_list[[1,2]]
resistance_y2 <- resistance_list[[2,2]]
y_resistance <- c(resistance_y1, resistance_y2)


df <- data.frame(time = index(twtr.p_NY),
                 a = twtr.p_NY$TWTR.Close)
df <- melt(df ,  id.vars = 'time', variable.name = 'series')

df_support <- data.frame(time = x_support, a = y_support)
df_support <- melt(df_support,  id.vars = 'time', variable.name = 'series')

df_resistance <- data.frame(time = x_resistance, a = y_resistance)
df_resistance <- melt(df_resistance,  id.vars = 'time', variable.name = 'series')


ggplot() +
  theme_bw() +
  geom_line(aes(time, value, colour = "TWTR"), df) +
  geom_line(aes(x_support, y_support, colour = "support"), df_support) +
  geom_line(aes(x_resistance, y_resistance, colour = "resistance"), df_resistance) +
  geom_abline(slope=lm(y_support ~ x_support)$coeff[[2]], intercept = lm(y_support ~ x_support)$coeff[[1]], colour="green") +
  geom_abline(slope=lm(y_resistance ~ x_resistance)$coeff[[2]], intercept = lm(y_resistance ~ x_resistance)$coeff[[1]], colour="orange")



#----------------------------------------5----------------------------------------#
# Plot the TWR.F graph
# Get the data, in the date range
twtr.p_DE <- getSymbols("TWR.F", env=NULL, src="alphavantage", api.key="3T47XKRJ6XUBRDFE",from = "2021-12-01", to = "2022-04-30")
# Plot the graph
lines(weeklyReturn(twtr.p_DE, subset="2021-12-01::2022-04-30"), col="red")


#----------------------------------------5b---------------------------------------#
# Find the peak
# List of all weekly returns
weeklyReturn(twtr.p_DE, subset="2021-12-01::2022-05-01")
# Peak
max(weeklyReturn(twtr.p_DE, subset="2021-12-01::2022-04-30"))


#----------------------------------------5c---------------------------------------#
# Compare the closure prices
# Get the data (in EUR) for TWR.F
closure_TWR.F <- coredata(Cl(to.daily(twtr.p_DE["2021-12-01::2022-04-30"])))
# Get the data (in USD) for TWTR
closure_TWTR <- coredata(Cl(to.daily(twtr.p_NY["2021-12-01::2022-04-30"])))
# Get the EUR/USD ratio - (quantmod::getFX returns only the last 180 days, so another provider is used here)

exchange_rate <- coredata(historical_exchange_rates("EUR", to = "USD",start_date = "2021-12-01", end_date = "2022-04-30"))
plot(exchange, type="l", main = "EUR / USD rate", panel.first=grid())
colnames(exchange_rate)[1] <- "date"                 #merge function needs to operate on the same column name
# Create a datatable that contains the following columns: TWR.F (EUR) - TWTR (EUR)
#   Given that the stock market is closed during the holidays, not all the exchange days are useful
dt_FR <- data.table(index(Cl(to.daily(twtr.p_DE["2021-12-01::2022-04-30"]))),coredata(Cl(to.daily(twtr.p_DE["2021-12-01::2022-04-30"]))))
colnames(dt_FR)[1] <- "date"
colnames(dt_FR)[2] <- "value"
dt_FR_allDays <- merge.data.table(exchange_rate, dt_FR, all = TRUE, by = "date")
dt_NY <- data.table(index(Cl(to.daily(twtr.p_NY["2021-12-01::2022-04-30"]))),coredata(Cl(to.daily(twtr.p_NY["2021-12-01::2022-04-30"]))))
colnames(dt_NY)[1] <- "date"
colnames(dt_NY)[2] <- "value"
dt_NY_allDays <- merge.data.table(exchange_rate, dt_NY, all = TRUE, by = "date")
#   NY data are in USD, convert them in EUR
dt_NY_allDays$EURvalues <- dt_NY_allDays$value / dt_NY_allDays$one_EUR_equivalent_to_x_USD
#   Create a dataframe with three variables: date, FR, NY_EUR
df_final <- data.frame(date=dt_FR_allDays$date, FR=dt_FR_allDays$value, NY=dt_NY_allDays$EURvalues)
#   Final plot
plot(df_final$date[4:length(df_final$date)-4], rollapplyr(df_final$FR, 5, mean, na.rm = TRUE, by = 1), type="l", main = "comparison TWTR FR-NY, EUR", col="red", panel.first=grid())
lines(df_final$date[4:length(df_final$date)-4], rollapplyr(df_final$NY, 5, mean, na.rm = TRUE, by = 1), type="l", col="blue")

# Verify if the two series are correlated, as it seems
# If the p-value is < 5%, then the correlation between x and y is significant
cor.test(rollapplyr(df_final$FR, 5, mean, na.rm = TRUE, by = 1), rollapplyr(df_final$NY, 5, mean, na.rm = TRUE, by = 1), method = c("pearson"))



#----------------------------------------6----------------------------------------#
# Find what happened that day
# Read the JSON (3 days before and 3 days after the date)
jsonname <- 'https://api.currentsapi.services/v1/search?keywords=Twitter&language=en&start_date=2022-04-05T00:00&end_date=2022-04-11T23:59&apiKey=qx7vZM47j1LnR9pt-LwglFZW60gcRyPl-QYVfOQOSAdlyTDU'
read_soruce <- fromJSON(file = jsonname)

# Create a dataset only for the "abstract" field
#   the supporting function removes the useless details from the JSON
abstract_set <- lapply(
  read_soruce$news,
  function(x) c(x['description'])
)
abstract_set <- do.call(rbind, abstract_set)


keystring = ""
for (i in 1:length(abstract_set)){
  keystring <- paste(keystring, abstract_set[[i]], sep=" ")
}
keystring.splitted <- strsplit(keystring, " ")
keystring.splitted <- keystring.splitted[[1]][nchar(keystring.splitted[[1]]) > 3]
common_words <- c("the", "be", "to", "of", "and", "a", "in", "that", "have", "I", "it", "for", "not", "on", "with", "he", "as", "you", "do", "at", "this", "but", "his", "by", "from", "they", "we", "say", "her", "she", "or", "an", "will", "my", "one", "all", "would", "there", "their", "what", "so", "up", "out", "if", "about", "who", "get", "which", "go", "me", "when", "make", "can", "like", "time", "no", "just", "him", "know", "take", "people", "into", "year", "your", "good", "some", "could", "them", "see", "other", "than", "then", "now", "look", "only", "come", "its", "over", "think", "also", "back", "after", "use", "two", "how", "our", "work", "first", "well", "way", "even", "new", "want", "because", "any", "these", "give", "day", "most", "us", "person", "thing", "man", "world", "life", "hand", "part", "child", "eye", "woman", "place", "week", "case", "point", "government", "company", "number", "group", "problem", "fact", "find", "tell", "ask", "seem", "feel", "try", "leave", "call", "last", "long", "great", "little", "own", "old", "right", "big", "high", "different", "small", "large", "next", "early", "young", "important", "few", "public", "bad", "same", "able")
keystring.splitted <- keystring.splitted[!keystring.splitted %in% common_words]
keystring.splitted <- sapply(strsplit(keystring.splitted, split='\''), function(x) (x[1]))
keystring.splitted <- sapply(strsplit(keystring.splitted, split='''), function(x) (x[1]))   #WARNING: if the typographic apostrophe (Unicode: U+2019) in the split argument is converted by RStudio to the standard apostrophe (provoking a compiling error), copy the typographic apostrophe from element 568 in keystring.splitted variable and paste it as right argument
keystring.splitted <- gsub("[^[:alnum:][:space:]]", "", keystring.splitted)
keystring.freq <-as.data.frame(table(keystring.splitted))
keystring.freq <- keystring.freq[order(keystring.freq$Freq,decreasing=TRUE),]
print(keystring.freq[1:5,]$keystring.splitted, max.levels = 0)   # the latter removes the level numbers


