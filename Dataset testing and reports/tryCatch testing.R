library(psych)
library(tidyverse)

test = c(5,4,3,3,2,2,1,NA)

ntests = 1000

test_store = NULL

for (i in 1:ntests) {
    curr_sample = sample(test, 3)
    
    curr_index = as.numeric(sample(c(1:5), 1))
    
    curr_mean = try(lm(curr_sample))
    
    if ("try-error" %in% class(curr_mean)) {
        curr_mean = NA
        test_store[i] = NA
    } else {
        test_store[i] = 1
    }
    
    curr_sum = curr_mean + 1
}

for (i in 1:ntests) {
    curr_sample = sample(test, 3)
    
    curr_mean = lm(curr_sample)
    
    test_store[i] = NA
}
