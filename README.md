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
test_proportions <- prop.test(m,n, alternative = "greater", correct = FALSE) #greater bo to right-sided test, correct should be false why? to najnormalniejsza wersja testu bez continuity correction, dlatego correct = FALSE
test_proportions #wykonuje funkcje pokazuje dane
sqrt(test_proportions$statistic) #podobnie 

^^^ x-squared jest tutaj inne niz w excelu, wiec trzeba napisac dodt linijjke kodu 

# Install and load the required R package "arules" to illustrate association rules
install.packages("arules")
library(arules)

# Read in the "Exercise_1_transactions.csv" data set and input it into object retail_data
retail_data <- read.transactions('/Users/patrycjahejmo/desktop/Exercise_2_vegetables_stand_data.csv', format = 'single', header =TRUE, cols=c("transaction_number" , "items"))
             #we just created a new object called "retail_data
# Make the data set into the required R transaction object
transactions_data <- as(retail_data, "transactions")
            #above function is called "as", and then retail data is the function we created in line 6, and then we write that we want to convert the data into transations data format
# Show a short summary of all transactions in the data set #its compulsory to conduct a market basket analysis
summary(transactions_data)
          # element (itemset/transaction) length distribution explained is 4 transactions with 1 item; 2 transactions with 4 items, 3 transactions with 1 item, 4 transactions with 1 item
# Show the first ten transactions (records) in the data set of all transactions (we see a few examples from the baskets)
inspect(head(transactions_data, 10))
           #transactions data is the object, and 10 cause we want to see the first 10 transactions (but we can make it any number smaller than 10); interpret support for this rule: 0.4 support means that 40% of all transactions contain joint probability, so they contain both ketchup and pizza
# Find association rules with the ?apriori? algorithm
# Use the parameter=list() control to instruct the algorithm to search rules that have a minimum support and a minimum confidence
# The resulting rule set is assigned to the association.rules.1 object 
#we have to set a minimal level of support and confidence
#minlen of association rules
association.rules.1 <- apriori(transactions_data, parameter = list(supp=0.20, conf=0.7, minlen=2, maxlen=10, target="rules"))        #first argument= transactions_data, second argument= parameter is in the form of a list, third argument is support (minimum level), minlen- minimum length- minimum number of items in the rule, maxlen- maximum number of items in the rule; the confidence- it is a conditional probability (of A given B); confidence of 1 means that 100% of transactions which contain ketchup also containp pizza; lift is equal to 2, so is greater than 1, which when it is greater than 1 means that those 2 items are positively related, which also means that thanks to this rule customers who buy ketchup are 2 times more likely to buy pizza as customers from the entire data set
           # generate association rules means find frequent patterns
# Show the first ten rules that have a minimum required conditions and sort them by support
inspect(head(sort(association.rules.1, by="supp"), 10))
          #rules: if ketchup, then pizza is bought; if pizza then ketchup; if chips then pizza (pizza is antecedent, pizza is the consequent)
# Now use the parameter=list() control to instruct the algorithm to search rules that have a minimum support and a minimum confidence
# The resulting rule set is assigned to the association.rules.2 object
association.rules.2 <- apriori(transactions_data, parameter = list(supp=0.20, conf=0.4, minlen=2, maxlen=10, target="rules")) 

# Show the first ten rules that have a minimum required conditions and sort them by lift
inspect(head(sort(association.rules.2, by="lift"), 10))

#lhs left-hand side, rhs right-hand side
#form o association form - if a, then b; anticident and consequent
install.packages("arulesViz")
library(arulesViz)
plot(association.rules.1)

association.rules.3 <- apriori(transactions_data, parameter = list(supp=0.1, conf=0.1, minlen=2, maxlen=10, target="rules"))
                               
inspect (head(sort(association.rules.3, by="supp"),10))
#20% of all transactions contain both beer and milk, confidence 66,7% of transactions which contain beer contain milk as well, lift - greater than 1 so it means that these two items are positevely related; thanks to this rule customers who buy if someone buys bear then they are 1,3 times more likely to buy milk 100% as customers from the entire data set
plot(association.rules.3)

rules.highest <- head(sort(association.rules.3, by="lift"),10)

inspect(rules.highest)
plot(rules.highest, method="graph")



# postaci binarne, broadcasy, IP itd z przedmiotu MONITOROWANIE I ZARZĄDZANIE SIECIAMI KOMPUTEROWYMI

Każdy adres składa się z cztrech oktetów (8 liiczb w b). Każdy z nich reprezentuje liczbe w wartości od 0 do 255. 

# 1. Osiedle i mieszkania – co to za IP?
Adres IP, np. 192.168.1.0/24, to osiedle z mieszkaniami:

192.168.1.0 – adres osiedla (początek adresu).
# /24 oznacza maskę: pierwsze 24 bity to ulica (sieć), a pozostałe bity to numer mieszkania (hosty).
Przy masce /24 masz 256 mieszkań w tym osiedlu (2 do potęgi 8 = 256).

Jeśli jest więcej bitów na hosty, to oznacza, że w tej sieci może być więcej użytkowników (hostów), które mogą jednocześnie korzystać z tej sieci.

Bity na sieć i bity na hosty

Bity na sieć: Określają, do jakiej sieci należy adres IP. Są stałe i identyczne dla wszystkich urządzeń w tej samej sieci.
Bity na hosty: Pozwalają przypisać adresy różnym urządzeniom w tej sieci. Każda unikalna kombinacja bitów hosta to inny komputer, drukarka, telefon itp.

python code - przekształcanie postaci zwykłej do postaci binarnej, IP ADDRESS

def ip_to_binary(ip_address):
    # Podziel adres IP na oktety
    octets = ip_address.split('.')
    # Przekształć każdy oktet na 8-bitowy kod binarny
    binary_octets = [format(int(octet), '08b') for octet in octets]
    # Połącz binarne oktety w wynikowy ciąg
    binary_ip = '.'.join(binary_octets)
    return binary_ip

# Przykład
ip_address = "192.168.0.1"
binary_result = ip_to_binary(ip_address)
print(f"Adres IP {ip_address} w binarnym: {binary_result}")

Kod binarny ma mieć zawsze 8 cyfr (dodaje zera z przodu, jeśli liczba ma mniej bitów).
