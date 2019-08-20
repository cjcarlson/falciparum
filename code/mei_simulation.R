# quick simulation of MEI

# vector of T
t = seq(from=5, to = 40, by=0.5)

# survival
p = exp(-1/(-4.4+1.31*t-0.03*t^2))
plot(t,p)

# MEI
a = 0.5
mei = (a^2*p^(111/(t-16)))/(-log(p))

# plot
plot(t,mei,type='l')

### other metric in McCord paper for sporogony: r
num = 0.06044*(t/296.65)*exp((17545/1.987)*(1/296.65 - 1/t))
den = 1 + exp((-142843/1.987)*(1/288.85 - 1/t)) + exp((110980/1.987)*(1/306.90 - 1/t))
r = num/den
plot(t,r) # hmmmmm can't be right

mei_alt = (a^2*p^r)/(-log(p))
plot(t,mei_alt)
