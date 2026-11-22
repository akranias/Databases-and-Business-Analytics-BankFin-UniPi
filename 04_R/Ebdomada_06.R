# =====================================================================
#  Εβδομάδα 6 · Εισαγωγή στην Οικονομετρία
#  Βάσεις Δεδομένων και Επιχειρηματική Αναλυτική · Πανεπιστήμιο Πειραιώς
#  Δεδομένα: UniStyle_Daily.csv — μία γραμμή ανά ημέρα (2024–2025)
#  Η εταιρεία και τα δεδομένα είναι ΦΑΝΤΑΣΤΙΚΑ, για εκπαιδευτική χρήση.
# =====================================================================
#  Πριν ξεκινήσετε: Session → Set Working Directory → To Source File Location
#  Ctrl + Enter: τρέχει τη γραμμή · Ctrl + Shift + Enter: όλο το script

# ---- 1. Τα δεδομένα ---------------------------------------------------
daily <- read.csv("UniStyle_Daily.csv")

head(daily)              # οι πρώτες 6 γραμμές (= SELECT TOP 6)
str(daily)               # οι στήλες και ο τύπος τους
nrow(daily)              # 731 ημέρες
summary(daily$Revenue)   # πωλήσεις ημέρας (€)

# Revenue       : καθαρές πωλήσεις της ημέρας (€, μόνο Delivered)
# Orders        : παραγγελίες της ημέρας
# SearchAds     : διαφήμιση σε μηχανές αναζήτησης (€)
# SocialAds     : διαφήμιση σε social media (€)
# AdSpend       : SearchAds + SocialAds (€)
# EmailCampaign : 1 αν στάλθηκε newsletter
# Weekend       : 1 για Σάββατο και Κυριακή
# BlackFriday   : 1 για τις 4 ημέρες της Black Friday κάθε χρονιάς

# ---- 1β. Ζέσταμα: περιγραφικά στατιστικά & χρονοσειρά ----------------
# Τύπος δεδομένων: ΧΡΟΝΟΣΕΙΡΑ — μία γραμμή ανά ημέρα, η σειρά έχει σημασία
summary(daily$Revenue)     # μέσος 446.13 > διάμεσος 348.94 → δεξιά ασυμμετρία
summary(daily$AdSpend)     # μέσος 96.88 · διάμεσος 84.62
sd(daily$Revenue)          # τυπική απόκλιση ≈ 385.40
cov(daily$AdSpend, daily$Revenue)   # συνδιακύμανση ≈ 17743.84

# Πίνακας συνδιακύμανσης & συσχέτισης (όλα τα ζεύγη μαζί)
vars <- c("Revenue", "SearchAds", "SocialAds")
cov(daily[, vars])              # διαγώνιος = διακυμάνσεις, π.χ. Revenue 148531.4
round(cor(daily[, vars]), 2)    # SearchAds–SocialAds ≈ 0.97 → εβδ. 8

# Από ημερήσια σε μηνιαία χρονοσειρά (= GROUP BY Year, Month της SQL)
monthly <- aggregate(Revenue ~ Year + Month, data = daily, FUN = sum)
m24 <- subset(monthly, Year == 2024)
m25 <- subset(monthly, Year == 2025)
plot(m25$Month, m25$Revenue, type = "b", col = "red", lwd = 2,
     ylim = c(0, 32000), xlab = "Μήνας", ylab = "Καθαρές πωλήσεις μήνα (€)")
lines(m24$Month, m24$Revenue, type = "b", col = "grey50", lwd = 2)
legend("topleft", legend = c("2025", "2024"), col = c("red", "grey50"), lwd = 2)
# Εποχικότητα: κορυφή Νοέμβριο–Δεκέμβριο · Τάση: το 2025 είναι ψηλότερα

# Αυτοσυσχέτιση: πωλήσεις σήμερα vs χθες
cor(daily$Revenue[-1], daily$Revenue[-nrow(daily)])   # ≈ 0.40

# ---- 2. Απλή παλινδρόμηση: Revenue ~ AdSpend -------------------------
cor(daily$AdSpend, daily$Revenue)            # συσχέτιση ≈ 0.70

model <- lm(Revenue ~ AdSpend, data = daily)
summary(model)                               # β1 ≈ 4.08 · R² ≈ 0.49

# Διάγραμμα διασποράς με την ευθεία της παλινδρόμησης
plot(daily$AdSpend, daily$Revenue, pch = 16, col = "grey50",
     xlab = "Δαπάνη διαφήμισης ημέρας (€)", ylab = "Πωλήσεις ημέρας (€)")
abline(model, col = "red", lwd = 2)

# Πρόβλεψη για μια ημέρα με 150 € διαφήμιση
predict(model, newdata = data.frame(AdSpend = 150))   # ≈ 663

# ---- 3. Πολλαπλή παλινδρόμηση ----------------------------------------
m1 <- lm(Revenue ~ AdSpend, data = daily)
m2 <- lm(Revenue ~ AdSpend + Weekend + EmailCampaign + BlackFriday, data = daily)

summary(m2)
coef(m2)                 # μόνο οι συντελεστές

# Πρόβλεψη: καθημερινή, 150 € διαφήμιση, με newsletter, όχι Black Friday
new_day <- data.frame(AdSpend = 150, Weekend = 0, EmailCampaign = 1, BlackFriday = 0)
predict(m2, newdata = new_day)                        # ≈ 738

# =====================================================================
#  ΕΡΓΑΣΤΗΡΙΟ
# =====================================================================

# ---- Άσκηση 1 · Πόσες πωλήσεις φέρνει η διαφήμιση; --------------------
# α) Επιβεβαιώστε τα νούμερα της διαφάνειας 13 (το model παραπάνω)

# β) Το ίδιο μοντέλο χωριστά για 2024 και 2025
d24 <- subset(daily, Year == 2024)       # subset() = WHERE της SQL
m <- lm(Revenue ~ AdSpend, data = d24)
coef(m)
summary(m)$r.squared
# ... τώρα το ίδιο για το 2025

# γ) Orders ~ AdSpend — πόσα € διαφήμισης «κοστίζει» μία επιπλέον παραγγελία;

# δ) Πρόβλεψη για ημέρα με 200 € διαφήμιση


# ---- Άσκηση 2 · Τι άλλο εξηγεί τις πωλήσεις; ---------------------------
# α) Μοντέλα 1 και 2 (m1, m2 παραπάνω)
# β) Ερμηνεία των AdSpend και EmailCampaign του m2
# γ) Πρόβλεψη: Σάββατο, 120 € διαφήμιση, χωρίς newsletter
# δ) Revenue ~ Weekend μόνο του — τι βγαίνει;


# ---- Άσκηση 3 · Challenge ---------------------------------------------
# OLS «με το χέρι»: β1 = Cov(x, y) / Var(x),  β0 = mean(y) - β1 * mean(x)
b1 <- cov(daily$AdSpend, daily$Revenue) / var(daily$AdSpend)
b0 <- mean(daily$Revenue) - b1 * mean(daily$AdSpend)
c(b0, b1)                # ίδια με coef(model);

# Google ή social media;
# lm(Revenue ~ SearchAds + SocialAds, data = daily)
