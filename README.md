# teoria r i excdel
Excel VBA pierwsze makro - funkcja developer excel, stworzenie funkcji, stworzenie przycisku uruchamiajacego makro, zapis plikow

# statystyka - classical approach and p-value
x <- read.csv2("/Users/patrycjahejmo/Desktop/Defective_wheelchairs.csv")
attach(x)
x
t.test(Day_shifts, Afternoon_shifts, var.equal=FALSE, alternative = "less")

# Tworzenie histogram
hist(x, main = "Histogram", xlab = "Sales", ylab = "afternoon")

# Two populations with proportions in R 
n <- c(175,150) #sample size z pierwszej grupy i drugiej
m  <- c(54,36)
test_proportions <- prop.test(m,n, alternative = "greater", correct = FALSE) #greater bo to right-sided test, correct should be false why? nie wiem
test_proportions #wykonuje funkcje pokazuje dane
sqrt(test_proportions$statistic) #podobnie 

^^^ x-squared jest tutaj inne niz w excelu, wiec trzeba napisac dodt linijjke kodu 


