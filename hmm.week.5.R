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

# berechne return eines Tages
x <- cbind(x, "RETURN"=(x$CLOSE_PRICE - x$OPEN_PRICE)/x$OPEN_PRICE);

#############################################

number_of_days <- 7;

# extrahiere ersten und letzten Tag der Woche
if (number_of_days > 1){
   x <- cbind(x, "WEEK_DAY"=((1:nrow(x))%%number_of_days));
   open <- x$OPEN_PRICE[x$WEEK_DAY==1];
   close <- x$CLOSE_PRICE[x$WEEK_DAY==0];
   open_date <- x$DATE[x$WEEK_DAY==1];
   close_date <- x$DATE[x$WEEK_DAY==0];
} else {
   open <- x$OPEN_PRICE;
   close <- x$CLOSE_PRICE;
   open_date <- x$DATE;
   close_date <- x$DATE;
}

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
set.seed(2);	# AIC = -591.36; 65 state changes; seems most likely 2018-12-01
#set.seed(5);	# AIC = -584.33; 99 state changes
#set.seed(6);	# AIC = -590.20; 81 state changes
#set.seed(7);	# AIC = -586.85; 71 state changes
hmm5 <- depmix(RETURN ~ 1, family = gaussian(), nstates = 5, data=data);
hmmfit5 <- fit(hmm5, em.control=em.control(maxit=5000, tol=1e-10), verbose = FALSE);

aic <- AIC(hmmfit5);
bic <- BIC(hmmfit5);
llk <- logLik(hmmfit5);

post_probs5 <- posterior(hmmfit5);
post_probs5 <- cbind(post_probs5, data$PRICE_);
post_probs5 <- cbind(post_probs5, data$DATE);
post_probs5 <- cbind(post_probs5, data$MA203);

# make weeks
weeks <- rep(NA, nrow(post_probs5));
for (i in 2011:2100){
   indices <- sort(which(grepl(paste("^", i, sep=""), post_probs5[,"data$DATE"])));
   weeks[indices] <- (52-length(indices) + 1):52;
}
weeks <- which(weeks %in% c(1,27));

rownames(post_probs5) <- NULL;
nsc <- sum(c(post_probs5[,1], NA)!=c(NA, post_probs5[,1]), na.rm=TRUE);

foo <- table(post_probs5[,1]);
foo <- foo[order(foo, decreasing=TRUE)];

colors <- rep(NA, 5);
bar <- c("red", "green", "black", "red4", "violet");
for (i in 1:length(foo))
   colors[as.numeric(names(foo)[i])] <- bar[i];

dev.new(width=19, height=3.5);
matplot(post_probs5[,-c(1,8)], type="l", col=c(colors, "blue", "orange"), lty = c(1,1,1,1,1,1,1), xaxt="n");

axis(1, at = weeks, labels = y$DATE[weeks]);
legend("left", legend = c("bubble", "bullish", "sideways", "bearish", "dead", "price", "MA203"), col = c("green", "violet", "black", "red", "red4", "blue", "orange"), lty = c(1,1,1,1,1,1,1), lwd = 1 , xpd = T );

title(paste("Five states HMM (", x$DATE[nrow(x)], "):\nlogLik = ", format(round(llk, 2), nsmall = 2), ", AIC = ", format(round(aic, 2), nsmall = 2), ", BIC = ", format(round(bic, 2), nsmall = 2), ", # of state changes = ", nsc, sep=""));

for (i in 1:nrow(post_probs5)){rect(i-1, 0, i, -0.1, col=colors[post_probs5[i,1]], border=NA)}
