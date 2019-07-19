library(zoo); # für sliding window
library(anytime); # für Zeitumrechnung
library(data.table); # für Tabellenkonvertierung
library('depmixS4'); # für HMMs

temp <- tempfile();
download.file("https://api.bitcoincharts.com/v1/csv/bitstampUSD.csv.gz", temp);
table <- read.csv(gzfile(temp));
unlink(temp);

rm(temp);
gc();

# Benennung der Spalten
colnames(table) <- c("EPOCH", "PRICE", "VOLUME");

# Anfügen einer neuen Spalten mit den Daten (Datums), da ansonsten Epochs
table <- cbind(table, "DATE"=anydate(table[,1]), "CEST");

DT <- data.table(table); # anderer Tabellentyp

# berechne den gewichteten Mittelwert des Preises eines Tages
x <- DT[,list(MEAN_PRICE = weighted.mean(PRICE,VOLUME)),by=DATE];
rm(DT);
gc();

# berechne den Eröffnungspreis eines Tages
open <- aggregate(PRICE ~ DATE, table, function(x){return(head(x, 1))});
# berechne den Schliessungspreis eines Tages
close <- aggregate(PRICE ~ DATE, table, function(x){return(tail(x, 1))});
rm(table);
gc();

# Spalten anfügen
x <- cbind(x, "OPEN_PRICE"=open$PRICE);
x <- cbind(x, "CLOSE_PRICE"=close$PRICE);
x <- cbind(x, "WEEK_DAY" = weekdays(x$DATE));
x <- x[which(grepl("Montag", x$WEEK_DAY))[1]:nrow(x),];

# remove all incomplete weeks
x <- x[81:nrow(x),];

# berechne return eines Tages
x <- cbind(x, "RETURN"=(x$CLOSE_PRICE - x$OPEN_PRICE)/x$OPEN_PRICE);

#############################################

open <- x$OPEN_PRICE[grepl("Montag", x$WEEK_DAY)];
close <- x$CLOSE_PRICE[grepl("Sonntag", x$WEEK_DAY)];
open_date <- x$DATE[grepl("Montag", x$WEEK_DAY)];
close_date <- x$DATE[grepl("Sonntag", x$WEEK_DAY)];

# passe die Länge der Vektoren an sodass es für jeden Tag eine Entsprechung gibt, einer der Fälle ist glaube ich unnötig
if (length(open) > length(close)) {open <- open[1:length(close)]; open_date <- open_date[1:length(close)];};
if (length(open) < length(close)) {close <- close[1:length(open)]; close_date <- close_date[1:length(open)];};

# erzeuge Datenframe für Wochen
y <- data.frame("OPEN_PRICE"=open, "CLOSE_PRICE"=close, "DATE"=paste(open_date, close_date, sep=" - "));
y <- cbind(y, "RETURN"=(y$CLOSE_PRICE - y$OPEN_PRICE)/y$OPEN_PRICE);

##############################################

# berechne "Schnitt" von Preis und füge diesen an Daten an
y <- cbind(y, "PRICE_" = (y$OPEN_PRICE+y$CLOSE_PRICE)/2);

# berechne logarithmischen Preis
y$PRICE_ <- log10(y$PRICE_);

# speichere Maximum von logarithmiertem Preis
mlp <- max(y$PRICE_);

# normalisiere Preis
y$PRICE_ <- y$PRICE_/mlp;

ma203 <- c(rep(NA, 28), rollapply((y$OPEN_PRICE + y$CLOSE_PRICE)/2, width=29, FUN=mean)); # MA203
y$MA203 <- ma203;

# berechne logarithmischen MA203
y$MA203 <- log10(y$MA203);

# normalisiere MA203
y$MA203 <- y$MA203/mlp;

##############################################

data <- y;

# different seeds generate several different lokal maxima, e.g. 3
# one of those is the wanted one but not necessarily with maximal
# log-likelyhood!
set.seed(3);
hmm4 <- depmix(RETURN ~ 1, family = gaussian(), nstates = 4, data=data);
hmmfit4 <- fit(hmm4, em.control=em.control(maxit=5000, tol=1e-10), verbose = FALSE);

aic <- AIC(hmmfit4);
bic <- BIC(hmmfit4);
llk <- logLik(hmmfit4);

post_probs4 <- posterior(hmmfit4);
post_probs4 <- cbind(post_probs4, data$PRICE_);
post_probs4 <- cbind(post_probs4, data$DATE);
post_probs4 <- cbind(post_probs4, data$MA203);

# make weeks
weeks <- rep(NA, nrow(post_probs4));
start_year <- 2011;
indices <- sort(which(grepl(paste("^", start_year, sep=""), post_probs4[,"data$DATE"])));
weeks[indices] <- (52-length(indices) + 1):52;

start_index <- max(which(!is.na(weeks))) + 1;
for (i in start_index:length(weeks)){
   weeks[i] <- (i - start_index) %% 52 + 1;
}
weeks <- which(weeks %in% c(1,27));

rownames(post_probs4) <- NULL;
nsc <- sum(c(post_probs4[,1], NA)!=c(NA, post_probs4[,1]), na.rm=TRUE);

foo <- table(post_probs4[,1]);
foo <- foo[order(foo, decreasing=TRUE)];

colors <- rep(NA, 4);
bar <- c("red", "black", "violet", "green");
for (i in 1:length(foo))
   colors[as.numeric(names(foo)[i])] <- bar[i];

dev.new(width=19, height=3.5);
matplot(post_probs4[,-c(1,7)], type="l", col=c(colors, "blue", "orange"), lty = c(1,1,1,1,1,1), xaxt="n");

axis(1, at = weeks, labels = y$DATE[weeks]);
legend("left", legend = c("bullish", "bubble", "sideways", "bearish", "price", "MA203"), col = c("green", "violet", "black", "red", "blue", "orange"), lty = c(1,1,1,1,1,1,1), lwd = 1 , xpd = T );

title(paste("Four states HMM (", x$DATE[nrow(x)], "):\nlogLik = ", format(round(llk, 2), nsmall = 2), ", AIC = ", format(round(aic, 2), nsmall = 2), ", BIC = ", format(round(bic, 2), nsmall = 2), ", # of state changes = ", nsc, sep=""));

for (i in (1:nrow(post_probs4))+1){rect(i-1, 0, i, -0.1, col=colors[post_probs4[i,1]], border=NA)}
