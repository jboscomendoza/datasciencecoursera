"4  incorrectas"

# Quiz 4
#
# 9 questions
# 1
# point
# 1.
#
# A pharmaceutical company is interested in testing a potential blood pressure lowering medication. Their first examination considers only subjects that received the medication at baseline then two weeks later. The data are as follows (SBP in mmHg)
# Subject	Baseline	Week 2
# 1	140	132
# 2	138	135
# 3	150	151
# 4	148	146
# 5	135	130
#
# Consider testing the hypothesis that there was a mean reduction in blood pressure? Give the P-value for the associated two sided T test.
#
# (Hint, consider that the observations are paired.)
#
# 0.05
#
# 0.10
#
#### 0.087
#
# 0.043

tablin <- data.frame(sujeto = 1:5, baseline = c(140, 138, 150, 148, 135), week2 = c(132, 135, 151, 146, 130))

t.test(tablin$baseline, tablin$week2, paired = T)

# 2.
#
# A sample of 9 men yielded a sample average brain volume of 1,100cc and a standard deviation of 30cc. What is the complete set of values of μ0 that a test of H0:μ=μ0 would fail to reject the null hypothesis in a two sided 5% Students t-test?
#
# 1081 to 1119
#
#### 1077 to 1123
#
# 1080 to 1120
#
# 1031 to 1169

1100 + c(-1, 1) * qt(0.975, 8) * 30/sqrt(9)

# 3.
#
# Researchers conducted a blind taste test of Coke versus Pepsi. Each of four people was asked which of two blinded drinks given in random order that they preferred. The data was such that 3 of the 4 people chose Coke. Assuming that this sample is representative, report a P-value for a test of the hypothesis that Coke is preferred to Pepsi using a one sided exact test.
#
##### 0.31
#
# 0.62
#
# 0.005
#
# 0.10

c(
    pbinom(1, size = 4, prob = .5, lower.tail = T),
    pbinom(3, size = 4, prob = .5, lower.tail = T)
)



# 4.
#
# Infection rates at a hospital above 1 infection per 100 person days at risk are believed to be too high and are used as a benchmark. A hospital that had previously been above the benchmark recently had 10 infections over the last 1,787 person days at risk. About what is the one sided P-value for the relevant test of whether the hospital is *below* the standard?
#
# 0.03
#
# 0.52
#
#0.11 (NO)
#
# 0.22

rate = 0.01
benchmark = 0.005595971
rate_casos = rate * 100
benchmark_casos = benchmark * 100

ppois(rate_casos, benchmark_casos)


# 5.
#
# Suppose that 18 obese subjects were randomized, 9 each, to a new diet pill and a placebo. Subjects’ body mass indices (BMIs) were measured at a baseline and again after having received the treatment or placebo for four weeks. The average difference from follow-up to the baseline (followup - baseline) was −3 kg/m2 for the treated group and 1 kg/m2 for the placebo group. The corresponding standard deviations of the differences was 1.5 kg/m2 for the treatment group and 1.8 kg/m2 for the placebo group. Does the change in BMI appear to differ between the treated and placebo groups? Assuming normality of the underlying data and a common population variance, give a pvalue for a two sided t test.
#
##### Less than 0.01
#
# Less than 0.05, but larger than 0.01
#
# Larger than 0.10
#
# Less than 0.10 but larger than 0.05

nt = 9
mediat = -3
desviaciont = 1.5
np = 9
mediap = 1
desviacionp = 1.8

sp <- sqrt( ((nt - 1) * desviaciont^2 + (np - 1) * desviacionp^2) / (nt + np - 2))

mediat - mediap + c(-1,1) * qt(.95, 16) * (desviaciont^2 / nt + desviacionp^2 / np)^.5

round(
    pt(q = c(-5.363579, -2.636421), df = (np+nt)-2)
, 2)

ts <- (x1 - x2)/(s * sqrt(1/n1 + 1/n2))
2 * pt(ts, n1 + n2 - 2)

# 6.
#
# Brain volumes for 9 men yielded a 90% confidence interval of 1,077 cc to 1,123 cc. Would you reject in a two sided 5% hypothesis test of
#
# H0:μ=1,078?
#
#### No you wouldn't reject.
#
# It's impossible to tell.
#
# Yes you would reject.
#
# Where does Brian come up with these questions?



# 7.
#
# Researchers would like to conduct a study of 100 healthy adults to detect a four year mean brain volume loss of .01 mm3. Assume that the standard deviation of four year volume loss in this population is .04 mm3. About what would be the power of the study for a 5% one sided test versus a null hypothesis of no volume loss?
#
# 0.70
#
# 0.60
#
### 0.80
#
# 0.50

power.t.test(n = 100, delta = 0.01, sd = 0.04, sig.level = 0.05, alternative = "one", type = "paired")


# 8.
#
# Researchers would like to conduct a study of n healthy adults to detect a four year mean brain volume loss of .01 mm3. Assume that the standard deviation of four year volume loss in this population is .04 mm3. About what would be the value of n needed for 90% power of type one error rate of 5% one sided test versus a null hypothesis of no volume loss?
#
# 120
#
# 180
#
####### 140
#
# 160

power.t.test(sd = 0.04, power = .9, sig.level = .05, delta = 0.01, alternative = "one", type = "paired")

# 9.
#
# As you increase the type one error rate, α, what happens to power?
#
### You will get larger power.
#
# No, for real, where does Brian come up with these problems?
#
# It's impossible to tell given the information in the problem.
#
# You will get smaller power.
# 7 questions unanswered

power.t.test(sd = 0.04, delta = 0.01, sig.level = 0.01, n = 200)
power.t.test(sd = 0.04, delta = 0.01, sig.level = 0.05, n = 200)
power.t.test(sd = 0.04, delta = 0.01, sig.level = 0.10, n = 200)
