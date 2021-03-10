
#maybe integrate this as an option in fitact, to save having to save the bootstrap pdfs on the offchance
# optional argument defining ranges of times over which to calculate activity
partAct1 <- function(pd, tt, range){
  if(range[1]>range[2]){ #if range overlaps midnight...
    tt[tt<range[2]] <- tt[tt<range[2]]+2*pi #increment lower times and second range value to make range increasing
    range[2] <- range[2]+2*pi
  }
  i <- tt>range[1] & tt<range[2] #indicator, are times within the range?
  sum(pd[i])*2*pi / (512 * (max(pd)*diff(range))) #return activity level
}

partAct2 <- function(rng, x, pdf, pdfs){
  act <- partAct1(pdf, x, rng)
  reps <- apply(pdfs, 2, partAct1, x, rng)
  se <- sd(reps)
  cl <- quantile(reps, c(0.025, 0.975))
  names(cl) <- c("lcl", "ucl")
  c(act=act, se=se, cl)
}

res <- cbind(rng,
             t(apply(rng, 1, partAct2, head(fit@pdf[,1], -1), head(fit@pdf[,2], -1), fit@pdfreps))
)
colnames(res) <- c("start", "stop", "act", "se", "lcl", "ucl")
res

sum(res[,"act"]*(1:2)/3)

library(activity)
data(BCItime)
fit <- fitact(subset(BCItime, species=="ocelot")$time*2*pi, sample="d")
dim(fit@pdfreps)
plot(fit)

1/(max(fit@pdf[,2])*2*pi)
fit@act
rng <- rbind(c(6, 18), c(18,6)) * pi/12
rng <- rbind(c(8, 16), c(16,8)) * pi/12

