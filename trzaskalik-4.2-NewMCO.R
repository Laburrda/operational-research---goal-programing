
# Instalacja biblioteki
if( !require(mco)) {install.packages(mco ,dependencies=TRUE)}
library(mco)

# Rozwiązanie przykladu 4.2 z ksiażki "Wprowadzenie do Badań Operacyjnych z Komputerem" T. Trzaskalik

# Dane w zadaniu
# Warunki brzegowe:
#  - x1 >= 0
#  - x2 >= 0
# Funkcje celu:
#  - f1(x1, x2) = 2x1 + 3x2 -> max
#  - f2(x1, x2) = 2x1 + 2x2 -> min
# Warunki ograniczające:
#  - x1 + 2x2 <= 8
#  - 4x1 <= 16

# Zadanie rozwiązujemy w następującej kolejności:

# 1. Funkcje Celu

# Za pomocą pakietu MCO możemy rozwiązać tylko przypadki z minimalizacją funkcji celu, w innej sytuacji potrzebujemy 
# dokonać przekształcenia warunków - mnożąc funkcję przez (-1). Gdy każda z naszych funkcji celu jest minimalizowana możemy 
# przystąpić do definiowania funkcji wektorowej:

fn <- function(x) {
  y <- numeric(2)                # długośc wektora 
  y[1] <- -(2 * x[1] + 3 * x[2]) # minimalizacja f1 ## dodając minus przed nawiasem rozwiązujemy problem maksymalizacji
  y[2] <- 2 * x[1] + 2 * x[2]    # minimalizacja f2
  return(y)
}

# 2. Warunki ograniczające + warunki brzegowe

# Do opisania warunków brzegowych i ograniczających potrzebujemy zrobić dwie rzeczy:
#  - wszystkie znaki nierówności muszą być ">=", w innej sytuacji mnożymy warunek przez (-1)
#    (w poleceniu zadania oba warunki ograniczające były znaku "<=")
#  - jeśli w warunkach po prawej stronie nierówności wystepuje inna wartośc niż "0" potrzebujemy przenieść ją na lewo (pamiętać o zmianie znaku)
# Gdy dokonaliśmy wszystkich transformacji możemy przystąpić do opisania warunków:

constraints <- function(x) {
  g <- numeric(4)                 # długośc wektora 
  g[1] <- -(x[1] + 2 * x[2]) + 8  # >= 0 w ten sposób przekształcilismy przykład ( x1 + 2x2 <= 8) do postaci odpowiadajacej programowi
  g[2] <- -(4 * x[1]) + 16        # >= 0 analogicznie jak wyżej
  g[3] <- x[1]                    # x1 >= 0 (spełnia już potrzeby programu)
  g[4] <- x[2]                    # x2 >= 0 (spełnia już potrzeby programu)
  return(g)
}

# 3. Dodatkowe zmienne (opcjonalne, można je zdefiniować wewnątrz funkcji)

lower_bounds <- c(0, 0)
upper_bounds <- c(10000, 10000)

# 4. Funkcja algorytmu NSGA II 

results <- nsga2(fn = fn,                     # tu podajemy zmienną z funkcjami celu
                 idim = 2,                    # wymiar wejściowy 
                 odim = 2,                    # wymiar wyjściowy (prawdopodobnie chodzi o ilość naszych x-ów na wejściu i wyjściu)
                 lower.bounds = lower_bounds, # dolny kranieć wartości x-ów
                 popsize = 720,               # ilość losowanych par (w konsoli może wyskoczyć komunikat, że musi być podzielna przez jakąś liczbę, jeśli tak będzie to zmeinić ilość w argumencie)
                 upper.bounds = upper_bounds, # górny kraniec wartości x_ów
                 constraints = constraints,   # tu podajemy warunki ograniczające i brzegowe
                 cdim = 4,                    # ilość wymiarów warunków (funkcji g)
                 cprob = 0.7,                 # *
                 cdist = 5,                   # *
                 generations = 100,           # *
                 mprob = 0.2,                 # *
                 mdist = 10,                  # *
                 vectorized = FALSE           # * - wszystkie kropki oznaczają standardowo zdefiniowane argumenty, do takich zadań nie trzeba nic zmieniać, w sytuacji bardziej skomplikowanej -> patrz dokumentacja
                 ) 

# 5. Odczytanie wyników

results$par # wszystkie pary x-ów (będzie ich tyle ile zdefiniowaliśmy przy argumencie popsize)

# Wykresy z dokumentacji pakietu MCO
opar <-par(mfrow=c(1,2))
plot(results, xlab="y1", ylab="y2", main="Objective space")       # wykres funkcji celu w wersji z minimalizacjami
plot(results$par, xlab="x1", ylab="x2", main="Parameter space")   # wykres wartości x-ów
par(opar)


# 6. Przekształcenie funkcji celu do treści zadania (jeśli wcześniej zmienialiśmy z maksymalizacji)


# Macierz zer potrzebna do dalszych obliczeń
f_celu <- matrix(0, ncol=ncol(results$par), nrow=nrow(results$par))

x <- results$par

# Obliczamy wartości funkcji celu zgodnie z poleceniem zadania
f_celu[,1] <- 2 * x[,1] + 3 * x[,2]   # pierwsza kolumna z wartościami pierwszej funkcji celu
f_celu[,2] <- -2 * x[,1] - 2 * x[,2]  # wartości drugiej funkcji celu (w przykładzie 4.2 wykres jest tworzony do wersji z dwiema maksymalizacjami)

colnames(f_celu) <- c("f1", "f2")

plot(f_celu, xlab="f1", ylab="f2", main="Rysunek 4.3 z podręcznika Trzaskalika")

# Rozwiązania odnoszą się do górnego brzegu funkcji celu. Wykres zgadza się z rysunkiem 4.3 z podręcznika. 
# Punk przełamania (12, -8) reprezentuje A', a naszy wykres jest stożkiem rozwiązań dominujących.

# Po uruchomieniu całego programu otrzymujemy trzy wykresy, można je przełączyć w R studio za pomocą strzałek (zakładka Plots)

