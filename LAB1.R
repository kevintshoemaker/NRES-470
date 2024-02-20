
x <- c(1,rep(2:20,each=2))
y = numeric(length(x)) 
y[1] = 30
for(i in 2:length(x)){
  if(i%%2){
    y[i] = y[i-1]*1.07
  }else{
    y[i] <- y[i-1]
  }
}
#y
plot(x,y,type="l",ylab="Abundance N",xlab="Time (years)",lwd=2,col="brown")


x <- 5     # define the variable 'x' as representing the value 5
x          # Print the value stored in the object "x"
r_d <- 0.1     #Assign the value of 0.1 to the object "r_d", or discrete per-capita growth rate (discrete)
lambda <- 1 + r_d           # (1 + r) is equal to lambda, the finite rate of growth.  This stores the result of the calculation (1 + 0.1 = 1.1) in the object "lambda".
N0 <- 100    #Assign the value of 100 to the object "N0", or initial population size
nyears <- 30 #Assign the value of 30 to the object "nyears", or the number of time steps to simulate
N0 * lambda   #Multiplies the value stored in the object "N0" by lambda. As soon as you run this line of code, the result of the calculation is printed.

year <- seq(from=0, to=nyears, by=1)   #Creates a sequence of numbers from 0 to the value stored in the object "nyears" (in this case, 30). Because you've told this sequence to increment by 1, you've created a string of numbers from 0 - 50 that contains 31 elements. A single series of elements (e.g., a single column of numbers) is called a vector. You then assign this vector to the object "years".

year                                   #Print the value of the object "year" that you just created.

vec_size <- nyears+1  # number of values to store is one more than nyears (includes year 0 all the way to year 50)
N <- numeric(vec_size)    #Make an empty storage vector. The numeric() function takes the contents within the parentheses and converts those contents to the "numeric" class. Don't worry if this doesn't make sense -- what you need to know is that the value within the parentheses (in this case 31) is used to tell this function how many zeros to create. So, this line of code creates a vector of 31 zeros, and assigns that vector to the object "N".

names(N) <- year

N                         #Prints the contents of the object "N".

N[1] <- N0                # The brackets [] are used to indicate the position of an element within a vector. This line of code assigns the value of the object "N0" (100) to the first element in the "N" object. Remember, the "N" object is a vector of 31 zeros representing year 0 to year 30. Now, the first zero is changed to 100.

for (i in 2:(nyears+1)){  # This for-loop will run through the line of code between the curly brackets {}. "i" is simply the name of a variable (you can use "j", or "k", instead -- any variable name will do). "i" changes each time the loop iterates; basically, it will increase by 1 each time the loop is run, starting at "2" up until the specified maximum number of loops "nyears+1". 
  N[i] <- N[i-1] * lambda  # This takes the [i - 1] element of "N", multiplies that element by the value of lambda, then assigns that calculated result to the [i] element of "N".
}                         # This ends the for-loop.

round(N)                         # Now print the contents of the object "N" (rounded).

N30 <- N0*lambda^nyears
round(N30)
plot(N~year)   #This plot() function tells R to plot the y variable by the x variable. "N" is the y variable (dependent variable), and "year" is the x variable (independent variable). The tilda "~" stands for "as a function of". There are many ways to customize the appearance of a plot in R - for now, just use the defaults.
