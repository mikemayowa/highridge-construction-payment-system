generate_workers <- function(num_workers) {
  workers <- list()
  genders <- c("Male", "Female")
  for (i in 1:num_workers) {
    name <- paste("Worker", i)
    gender <- sample(genders, 1)
    salary <- runif(1, 5000, 40000)
    workers[[i]] <- list(name = name, gender = gender, salary = salary)
  }
  return(workers) 
  }

assign_employee_level <- function(worker) {
  if (worker$salary > 10000 & worker$salary < 20000) {
    worker$employee_level <- "A1"
  } else if (worker$salary > 7500 & worker$salary < 30000 & worker$gender == "Female") {
    worker$employee_level <- "A5-F"
  }
}

generate_payment_slips <- function(workers) {
  for (worker in workers) {
    tryCatch({
      assign_employee_level(worker)
      cat("Name:", worker$name, ", Salary: $", worker$salary, ", Employee Level:", worker$employee_level, "\n")
    }, error = function(e) {
      cat("Error occurred for", worker$name, ":", conditionMessage(e), "\n")
    })
  }
}

num_workers <- 400
workers <- generate_workers(num_workers)
generate_payment_slips(workers)