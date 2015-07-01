library(snb)

s = 3
t = 5
p=0.5
flips = c(0, 0, 1, 0, 0, 1, 0)
pl = zplot(flips, s=s, t=t)

ggsave("../z-plot1.pdf", pl, width=5, height=3)

