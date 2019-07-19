library('depmixS4');

simulate <- function(state, tm, mvs, sds, op, n){
   ret <- matrix(NA, n+1, 3);
   colnames(ret) <- c("STATE", "OPENING_PRICE", "CLOSING_PRICE");
   ret[1,1] <- state;
   ret[1,2] <- op;

   tm_ <- tm;
   for (i in 1:nrow(tm))
      for (j in 2:ncol(tm))
         tm_[i,j] <- tm_[i,j-1] + tm_[i,j];
   tm_ <- cbind(0, tm_);

   for (i in 1:n){
      r <- rnorm(1, mvs[state], sds[state]);
      cp <- op/(1 - r);
      if (cp < 0)
         break;
      ret[i,3] <- cp;

      tps <- tm_[state,];
      rn <- runif(1);
      for (j in 2:length(tps))
         if (tps[j-1] <= rn && rn < tps[j])
            break;
      state <- j-1;

      op <- cp;
      ret[i+1,1] <- state;
      ret[i+1,2] <- op;
   }

   return(ret);
}

simulateN <- function(start_price = NULL, start_state = 1, n = 1, N = 10^6, fit_file = NULL, theoretical=FALSE, verbose=FALSE){
   if (is.null(start_price))
      error("Error: no start price provided");

   if (is.null(fit_file))
      error("ERROR: no file provided");

   percent <- round(N/100);

   fit <- get(load(fit_file));

   A <- t(matrix(getpars(fit), 4,7));
   tm <- A[2:5,1:4];

   mvs <- c(A[6,1], A[6,3], A[7,1], A[7,3]);
   sds <- c(A[6,2], A[6,4], A[7,2], A[7,4]);

   S <- rep(NA, N);
   for (i in 1:N){
      s <- simulate(start_state, tm, mvs, sds, start_price, n);
      S[i] <- s[nrow(s),2];
      if (verbose && i %% percent == 0){
         cat(round(i/percent)); cat(" ");
      }
   }

   S[is.na(S)] <- 0;
   median_price <- 10^median(log10(S), na.rm=TRUE);
   h <- hist(log10(S), 100, main=paste("log10 of price of ", N, " sampled markov chains\nmedian price = ", median_price, " $ after ", n, " time intervals,\nin ", 100*sum(S==0)/N, " % of runs, Bitcoin reaches a price of 0 $", sep=""), col="grey", border="grey");

   S <- S[S > 0];
   ints <- seq(0.05,0.95,0.05);
   if (!theoretical){
      S <- sort(S);
      prices <- S[round(N*ints)];
   } else {
      x <- seq(min(log10(S)),max(log10(S)), 0.001);
      y <- dnorm(x, mean(log10(S)), sd(log10(S)));
      y <- y*max(h$counts)/max(y);
      lines(x,y, col="blue")

      prices <- 10^(qnorm(ints, mean(log10(S)), sd(log10(S))));
   }
   names(prices) <- ints;
   
   sd <- sd(log10(S));
   for (i in 1:length(prices)){
      col <- "blue";
      if (i == ceiling(length(prices)/2))
         col <- "red";
      abline(v=log10(prices[i]), col=col);

      v = max(h$counts) - (1/20)*max(h$counts) - ((i-1)%%3)*(1/10)*max(h$counts);
      text(log10(prices[i]), v, paste(as.character(names(prices)[i]), "\n($ ", round(prices[i]), ")", sep=""), col="black", cex=0.5);
   }

   return(prices);
}

simulateNn <- function(start_price = NULL, start_state = 1, n = 1, N = 10^6, fit_file = NULL, verbose=FALSE){
   ret <- NULL;
   for (i in 1:n){
      ret <- rbind(ret, simulateN(start_price=start_price, start_state=start_state, n=i, N=N, fit_file=fit_file, verbose=verbose));
      if (verbose)
         cat("\n");
   }
   return(ret);
}

#library(ggplot2);
#X <- c(); for (i in 1:nrow(x)){X <- c(X, x[i,]);}; X <- cbind(X, rep(1:nrow(x), each=ncol(x))); X <- cbind(X, rep(colnames(x), nrow(x))); colnames(X) <- c("PRICE", "NO", "INT"); X <- as.data.frame(X); X$PRICE <- as.numeric(as.character(X$PRICE)); X$NO <- as.numeric(as.character(X$NO)); X$INT <- as.factor(X$INT); rownames(X) <- NULL;
#ggplot(X, aes(x=NO, y=LOGPRICE, color=INT, group=INT)) + geom_line() + geom_point();

#price <- 7000; weeks <- 33; n <- 1000000; S <- rep(NA, n); for (i in 1:n){s <- simulate(2,tm,mvs,sds,price,weeks); S[i] <- s[nrow(s),2]; if (i %% 10000 == 0) print(i)};
#S[is.na(S)] <- 0;
#median_price <- 10^median(log10(S), na.rm=TRUE);
#hist(log10(S), 100, main=paste("log10 of price of 10^6 sampled markov chains\nmedian price = ", median_price, " $ after ", weeks, " weeks,\nin ", 100*sum(S==0)/n, " % of runs, Bitcoin reaches a price of 0 $", sep=""))

#ret <- simulateN(start_price=10600, start_state=4, n = 1, N = 10^6, fit_file = "fit.2019.19.07.RData", theoretical=FALSE, verbose=FALSE);
#pie(c(seq(0.05,0.5,0.05), seq(0.45,0.05,-0.05)), labels=round(ret), clockwise=TRUE, col=colorRampPalette(c('red', 'black', 'green', 'violet'), alpha = FALSE)(19), main="Pie chart of potential BTC prices for 2019-07-21\nbased on data until 2019-07-14");
