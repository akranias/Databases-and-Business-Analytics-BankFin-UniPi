# =====================================================================
#  Εβδομάδα 6 · Εργαστήριο · Πρώτα βήματα στο RStudio
#  Βάσεις Δεδομένων και Επιχειρηματική Αναλυτική · Πανεπιστήμιο Πειραιώς
#  Δεδομένα: φάκελος UniStyle_CSV (Customers, Orders, OrderItems,
#            Products, Cities, UniStyle_Daily) — τα ίδια με την UniStyleDB
#  Η εταιρεία και τα δεδομένα είναι ΦΑΝΤΑΣΤΙΚΑ, για εκπαιδευτική χρήση.
# =====================================================================
#  Πριν ξεκινήσετε:
#   1. Βάλτε αυτό το script στον ίδιο φάκελο με τα CSV
#   2. Session → Set Working Directory → To Source File Location
#   3. Ctrl + Enter (Mac: Cmd + Enter) τρέχει τη γραμμή του κέρσορα

# ---- 1. Η R ως αριθμομηχανή ------------------------------------------
2 + 3
sqrt(144)
getwd()                       # ο φάκελος όπου «κοιτάει» η R

# ---- 2. Αντικείμενα & διανύσματα -------------------------------------
sales <- c(120, 340, 90)      # c() = «συνδύασε»
sum(sales)                    # 550
mean(sales)                   # 183.3333
length(sales)                 # 3
sales[2]                      # 340 — η αρίθμηση ξεκινά από το 1
sales * 1.24                  # πράξη σε όλα τα στοιχεία μαζί

# ---- 3. Τύποι δεδομένων ----------------------------------------------
class(446.13)                 # "numeric"   ~ DECIMAL
class("Athens")               # "character" ~ NVARCHAR
class(TRUE)                   # "logical"   ~ BIT
class(as.Date("2025-11-28"))  # "Date"      ~ DATE
# NA = το NULL της SQL

# ---- 4. Data frame: ο «πίνακας» της R ---------------------------------
daily <- read.csv("UniStyle_Daily.csv")
nrow(daily)                   # 731   ~ SELECT COUNT(*)
names(daily)                  # οι στήλες
head(daily, 3)                # ~ SELECT TOP 3 *
str(daily)                    # ο τύπος κάθε στήλης
mean(daily$Revenue)           # 446.1252 — το $ «ανοίγει» μια στήλη
round(mean(daily$Revenue), 1)

# ---- 5. Πακέτα ---------------------------------------------------------
# install.packages("dplyr")   # ΜΙΑ φορά ανά υπολογιστή (βγάλτε το # για να τρέξει)
library(dplyr)                # ΚΑΘΕ φορά που ανοίγετε το RStudio

# ---- 6. Εισαγωγή των πινάκων της UniStyleDB ---------------------------
customers <- read.csv("Customers.csv", na.strings = "")   # τα κενά → NA
orders    <- read.csv("Orders.csv")
items     <- read.csv("OrderItems.csv")
products  <- read.csv("Products.csv")

str(orders)
sum(is.na(customers$BirthDate))     # πελάτες χωρίς ημερομηνία γέννησης

# Αν ένα CSV είναι αποθηκευμένο από ελληνικό Excel (; και δεκαδικό κόμμα):
# x <- read.csv2("arxeio.csv", fileEncoding = "UTF-8")
# Απευθείας από Excel:
# x <- readxl::read_excel("UniStyle_eShop_Data.xlsx", sheet = "Orders")

# ---- 7. SQL → σκέτη R --------------------------------------------------
# Καθαρές πωλήσεις 2025 ανά κατηγορία
df <- merge(items, orders, by = "OrderID")           # INNER JOIN
df <- merge(df, products, by = "ProductID")          # INNER JOIN

df$NetAmount <- df$Quantity * df$UnitPrice * (1 - df$DiscountPct)
df$Year <- as.numeric(format(as.Date(df$OrderDate), "%Y"))

d25 <- subset(df, Status == "Delivered" & Year == 2025)            # WHERE

agg <- aggregate(NetAmount ~ Category, data = d25, FUN = sum)      # GROUP BY
agg$NetAmount <- round(agg$NetAmount, 2)
agg <- agg[order(-agg$NetAmount), ]                                # ORDER BY DESC
head(agg, 3)                                                       # Jackets 45782.34

# ---- 8. Το ίδιο με dplyr ------------------------------------------------
agg2 <- items |>
  inner_join(orders,   by = "OrderID") |>
  inner_join(products, by = "ProductID") |>
  mutate(NetAmount = Quantity * UnitPrice * (1 - DiscountPct),
         Year = as.numeric(format(as.Date(OrderDate), "%Y"))) |>
  filter(Status == "Delivered", Year == 2025) |>
  group_by(Category) |>
  summarise(NetSales = sum(NetAmount), Lines = n()) |>
  arrange(desc(NetSales))
agg2

# ---- 9. LEFT JOIN & NULL ------------------------------------------------
# Πελάτες χωρίς καμία παραγγελία
m <- merge(customers, orders, by = "CustomerID", all.x = TRUE)    # LEFT JOIN
sum(is.na(m$OrderID))                                             # IS NULL

customers |> anti_join(orders, by = "CustomerID") |> nrow()       # το ίδιο

# ---- 10. Γρήγορα γραφήματα ---------------------------------------------
hist(daily$Revenue, breaks = 30)
plot(daily$AdSpend, daily$Revenue)
barplot(agg$NetAmount, names.arg = agg$Category, las = 2)


# =====================================================================
#  ΑΣΚΗΣΗ Α · Από τα CSV στην ανάλυση (20 λεπτά)
#  Τιμές ελέγχου στη διαφάνεια 22
# =====================================================================

# α) Πόσες γραμμές έχει καθένας από τους 4 πίνακες;


# β) Πόσοι πελάτες δεν έχουν BirthDate; Δοκιμάστε και χωρίς na.strings = "".
#    Γιατί διαφέρει το αποτέλεσμα;


# γ) Καθαρές πωλήσεις 2025 (Delivered) ανά κανάλι (Channel):
#    μία φορά με aggregate() και μία με dplyr


# δ) Πόσοι πελάτες δεν έχουν καμία παραγγελία;


# ε) Μπόνους: μέση αξία παραγγελίας (καλαθιού) ανά κανάλι το 2025
#    (πρώτα άθροισμα ανά OrderID, μετά μέσος όρος)


# Μετά: Ebdomada_06.R — ασκήσεις παλινδρόμησης 1–3
