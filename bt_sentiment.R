library(anytime);

# import all comment-dates from dates.txt and reformat it
dates <- as.character(read.table(file="dates.txt", sep=";")[,1]);
dates <- t(matrix(unlist(strsplit(dates, ", ")), nrow=3, ncol=length(dates)));
# create a kind of day id
dates <- cbind(dates, "ID"=paste(dates[,1], dates[,2], sep=" "));
# add a column, where the day id is a real date (for future analyses)
ret <- as.character(anytime(dates[,"ID"]));
dates <- cbind(dates, "ID2"=ret);

# compute day table, i.e. number of comments per day
dates_table <- table(dates[,"ID2"]);

# plot distribution of comments over time
dev.new();
plot(dates_table, main="'Der Aktuelle Kursverlauf'-thread: comments/day");

# make a new plot
dev.new();

# create the distribution of comments per day
h <- hist(log(dates_table), xlim=c(-2, 10), main="'Der Aktuelle Kursverlauf'-thread: distribution of log(comments/day)");

# fit a normal distribution to the logarithmized
# distribution of comments per day
mu <- mean(log(dates_table));
sigma <- sd(log(dates_table));
x <- seq(mu-5*sigma, mu+5*sigma, 0.1);
y <- dnorm(x, mu, sigma);
y <- y/max(y);
y <- y*max(h$counts);
lines(x, y, type="l", col="blue");

# it seems, the logarithmized distribution is normal!

# we will treat the number of comments per day as a kind of "price" per day
bla <- as.data.frame(t(dates_table));
bla <- bla[,-1];
colnames(bla)[1] <- "DATE";

###############################################################################################
# we will use the value of monday as "opening price" and the value of sonday as "closing price"

# firstly, fill in missing days
ret <- NULL;
for (i in 1:(nrow(bla)-1)){
  ret <- rbind(ret, bla[i,]);
  diff <- as.numeric(as.Date(bla$DATE[i+1]) - as.Date(bla$DATE[i]));
  if (diff > 1){
    add <- matrix(NA, diff-1, ncol(bla));
    colnames(add) <- colnames(bla);
    for (j in 1:nrow(add)){
      add[j,"DATE"] <- as.character(as.Date(bla$DATE[i])+j);
    }
    ret <- rbind(ret, add);
  }
}
ret <- rbind(ret, bla[nrow(bla),]);
colnames(ret)[2] <- "PRICE";
rownames(ret) <- NULL;
ret$PRICE[is.na(ret$PRICE)] <- "0";
ret$PRICE <- as.numeric(ret$PRICE);

# add one to comments per day so that we won't have problems later on
# i.e. we want to be able to compute the logarithm even from days with no comment
# as well we want to be able to compute ratios with day with no comment
ret$PRICE <- ret$PRICE + 1;

# you never stop learning ;-)
wdays <- weekdays(as.Date(ret$DATE, "%Y-%m-%d"));
ret <- cbind(ret, "WEEK_DAY"=wdays);

# test if there is a Sunday before a Monday in the first seven days, if so, remove those rows
# since we always want to start with a Monday
m <- which(ret$WEEK_DAY=="Monday")[1];
s <- which(ret$WEEK_DAY=="Sunday")[1];
if (s < m)
  ret <- ret[m:nrow(ret),];

# test if there is a Monday after a Sunday in the last seven days, if so, remove those rows
# since we always want to end with a Sunday
m <- tail(which(ret$WEEK_DAY=="Monday"),1);
s <- tail(which(ret$WEEK_DAY=="Sunday"),1);
if (s < m)
  ret <- ret[1:s,];

# add a vector with week id
ret <- cbind(ret, "WEEK"=rep(1:(nrow(ret)/7), each=7));

# compute the mean of comments per day per week
ret_week <- aggregate(PRICE ~ WEEK, ret, mean);

# now get the opening and closing prices
opening_price <- ret[ret$WEEK_DAY=="Monday", "PRICE"];
closing_price <- ret[ret$WEEK_DAY=="Sunday", "PRICE"];

# merge it, dates will be defined by Mondays
ret_week <- cbind(ret_week, "DATE"=ret$DATE[ret$WEEK_DAY=="Monday"]);
ret_week <- cbind(ret_week, "OPEN_PRICE"=opening_price);
ret_week <- cbind(ret_week, "CLOSE_PRICE"=closing_price);

# compute return
ret_week <- cbind(ret_week, "RETURN" = (ret_week$CLOSE_PRICE - ret_week$OPEN_PRICE)/ret_week$OPEN_PRICE);

# make return plot
dev.new(width=19, height=3.5);
matplot(ret_week$RETURN, type="l", main="'Der Aktuelle Kursverlauf'-thread: return/week of comments", xaxt="n");
nweeks <- 10;
int <- floor(nrow(ret_week)/nweeks);
axis(1, at = int*(0:nweeks), labels = as.character(ret_week$DATE[int*(0:nweeks)+1]));
abline(h=0);

# compute hmm
library(depmixS4);
hmm <- depmix(PRICE ~ 1, family = gaussian(), nstates = 2, data=ret_week);
hmmfit <- fit(hmm, verbose = FALSE);
post_probs <- posterior(hmmfit);

# create plot
dev.new(width=19, height=3.5);
matplot(post_probs[,-1], type="l", col=c("darkgreen", "red"), lty = c(1,1), xaxt="n");
nweeks <- 10;
int <- floor(nrow(ret_week)/nweeks);
axis(1, at = int*(0:nweeks), labels = as.character(ret_week$DATE[int*(0:nweeks)+1]));
legend("left", legend = c("state 1", "state 2"), col = c("darkgreen", "red"), lty = c(1,1), lwd = 1 , xpd = T );
title("'Der Aktuelle Kursverlauf'-thread: comments-HMM with two states");
for (i in 1:nrow(post_probs)){rect(i-1, -0.01, i, -0.11, col=c("darkgreen", "red")[post_probs[i,1]], border=NA)}