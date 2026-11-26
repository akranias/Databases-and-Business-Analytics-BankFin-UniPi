# =====================================================================
#  Εβδομάδα 8 · Μπορώ να εμπιστευτώ το μοντέλο; Διαγνωστικοί έλεγχοι
#  Βάσεις Δεδομένων και Επιχειρηματική Αναλυτική · Πανεπιστήμιο Πειραιώς
#  Δεδομένα: UniStyle_Daily.csv (φανταστικά, για εκπαιδευτική χρήση)
# =====================================================================
#  Μία φορά: install.packages(c("car", "lmtest", "sandwich"))

library(car)        # vif()
library(lmtest)     # bptest(), dwtest(), coeftest()
library(sandwich)   # vcovHC()

daily <- read.csv("UniStyle_Daily.csv")
m2 <- lm(Revenue ~ AdSpend + Weekend + EmailCampaign + BlackFriday, data = daily)

# ---- 1. Πολυσυγγραμμικότητα -------------------------------------------
cor(daily[, c("SearchAds", "SocialAds", "EmailCampaign")])     # 0.97 !
m3 <- lm(Revenue ~ SearchAds + SocialAds + Weekend + EmailCampaign + BlackFriday,
         data = daily)
summary(m3)
vif(m3)             # ≈ 17 για τα δύο κανάλια
vif(m2)             # με το AdSpend: όλα κοντά στο 1

# ---- 2. Ετεροσκεδαστικότητα -------------------------------------------
plot(m2, which = 1)                                  # η «χοάνη»
bptest(m2)                                           # BP ≈ 37.1, p < 0.001
coeftest(m2, vcov = vcovHC(m2, type = "HC1"))        # robust τυπικά σφάλματα

# Ο έλεγχος H0: β = 2 με robust se
se_rob <- sqrt(diag(vcovHC(m2, type = "HC1")))["AdSpend"]
(coef(m2)["AdSpend"] - 2) / se_rob                   # t ≈ 8.26

# ---- 3. Αυτοσυσχέτιση -------------------------------------------------
dwtest(m2)                                           # DW ≈ 1.72
tapply(residuals(m2), daily$Month, mean)             # μέσο κατάλοιπο ανά μήνα

m4 <- lm(Revenue ~ AdSpend + Weekend + EmailCampaign + BlackFriday +
           factor(Month) + Year, data = daily)
summary(m4)
dwtest(m4)                                           # DW ≈ 2.01
coeftest(m4, vcov = vcovHC(m4, type = "HC1"))        # το τελικό μας μοντέλο

# Όλα τα διαγνωστικά διαγράμματα μαζί
par(mfrow = c(2, 2)); plot(m4); par(mfrow = c(1, 1))

# =====================================================================
#  ΕΡΓΑΣΤΗΡΙΟ
# =====================================================================
# Άσκηση 1 · Πολυσυγγραμμικότητα
#   α) cor(daily[, c("SearchAds", "SocialAds", "AdSpend", "EmailCampaign")])
#   β) m3 και vif(m3)    γ) με AdSpend αντί για τα δύο κανάλια
#   δ) μόνο SearchAds / μόνο SocialAds (μαζί με Weekend, EmailCampaign, BlackFriday)

# Άσκηση 2 · Ετεροσκεδαστικότητα
#   α) plot(m2, which = 1)   β) bptest(m2)   γ) coeftest(… "HC1")
#   δ) H0: β = 2 με το robust se

# Άσκηση 3 · Αυτοσυσχέτιση & εποχικότητα
#   α) dwtest(m2)   β) tapply(residuals(m2), daily$Month, mean)
#   γ) m4           δ) σύγκριση β, adjusted R², DW
