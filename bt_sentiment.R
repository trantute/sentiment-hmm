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
plot(dates_table);

# make a new plot
dev.new();

# create the distribution of comments per day
h <- hist(log(dates_table), xlim=c(-2, 10));

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