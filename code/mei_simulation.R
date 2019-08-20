# quick simulation of MEI

# vector of T
t = seq(from=5, to = 40, by=0.25)

# survival
p = exp(-1/(-4.4+1.31*t-0.03*t^2))
plot(t,p)

# MEI
a = 0.5
mei = (a^2*p^(111/(t-16)))/(-log(p))

plot(t,mei,type='l')
plot(t,p^(111/(t-16)))
