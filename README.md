# teoria r i excdel
Excel VBA pierwsze makro - funkcja developer excel, stworzenie funkcji, stworzenie przycisku uruchamiajacego makro, zapis plikow

# statystyka - classical approach and p-value
x <- read.csv2("/Users/patrycjahejmo/Desktop/Defective_wheelchairs.csv")
attach(x)
x
t.test(Day_shifts, Afternoon_shifts, var.equal=FALSE, alternative = "less")

# Tworzenie histogram
hist(x, main = "Histogram", xlab = "Sales", ylab = "afternoon")
