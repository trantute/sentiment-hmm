load("fit.RData"); # needs a fitted and saved hmm

A <- t(matrix(getpars(hmmfit4), 4,7));
tm <- A[2:5,1:4];

mvs <- c(A[6,1], A[6,3], A[7,1], A[7,2]);
sds <- c(A[6,2], A[6,4], A[7,2], A[7,4]);

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

price <- 7000; weeks <- 33; n <- 1000000; S <- rep(NA, n); for (i in 1:n){s <- simulate(2,tm,mvs,sds,price,weeks); S[i] <- s[nrow(s),2]; if (i %% 10000 == 0) print(i)};
S[is.na(S)] <- 0;
median_price <- 10^median(log10(S), na.rm=TRUE);
hist(log10(S), 100, main=paste("log10 of price of 10^6 sampled markov chains\nmedian price = ", median_price, " $ after ", weeks, " weeks,\nin ", 100*sum(S==0)/n, " % of runs, Bitcoin reaches a price of 0 $", sep=""))
