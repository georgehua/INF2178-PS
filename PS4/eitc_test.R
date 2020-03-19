# Create two additional dummy variables to indicate before/after
# and treatment/control groups.

# the EITC went into effect in the year 1994
eitc$post93 = as.numeric(eitc$year >= 1994)

# The EITC only affects women with at least one child, so the
# treatment group will be all women with children.
eitc$anykids = as.numeric(eitc$children >= 1)

# Compute the four data points needed in the DID calculation:
control_before = sapply(subset(eitc, post93 == 0 & anykids == 0, select=work), mean)
treatment_before = sapply(subset(eitc, post93 == 0 & anykids == 1, select=work), mean)
control_after = sapply(subset(eitc, post93 == 1 & anykids == 0, select=work), mean)
treatment_after = sapply(subset(eitc, post93 == 1 & anykids == 1, select=work), mean)

# Compute the effect of the EITC on the employment of women with children:
(treatment_after-control_after)-(treatment_before-control_before)

eitc$p93kids.interaction = eitc$post93*eitc$anykids
reg1 = lm(work ~ post93 + anykids + p93kids.interaction, data = eitc)
summary(reg1)