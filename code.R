library(dplyr)
library(magrittr)
library(ggplot2)
library(scales)

# Data downloading --------------------------------------------------------

# Полная государственная роспись расходов и доходов бюджета за 1912 год
budget.1912.url <- "http://minfin.ru/OpenData/7710168360-Raw-Budget-1912/data-01-structure-01.csv"
budget.1912.codebook.url <- "http://minfin.ru/OpenData/7710168360-Raw-Budget-1912/structure-01-20141230.csv"

# Бюджеты 1912-1915 годов по предметам расходов
budget.1912_15.by.spendings.url <- "http://minfin.ru/OpenData/7710168360-functional_budget_1912_1915/data-01-structure-01.csv"
budget.1912_15.by.spendings.codebook.url <- "http://minfin.ru/OpenData/7710168360-functional_budget_1912_1915/structure-01-20141230.csv"

# Бюджеты 1912-1915 годов по ведомствам
budget.1912_15.by.depart.url <- "http://minfin.ru/OpenData/7710168360-agencies_budget_1912_1915/data-01-structure-01.csv"
budget.1912_15.by.depart.codebook.url <- "http://minfin.ru/OpenData/7710168360-agencies_budget_1912_1915/structure-01-20141230.csv"

dir.create(file.path(".", "data"), showWarnings = FALSE)

src <- data.frame(files = c("budget.1912.csv",
                            "budget.1912.codebook.csv",
                            "budget.1912_15.by.spendings.csv",
                            "budget.1912_15.by.spendings.codebook.csv",
                            "budget.1912_15.by.depart.csv",
                            "budget.1912_15.by.depart.codebook.csv"),
                  urls = c(budget.1912.url,
                           budget.1912.codebook.url,
                           budget.1912_15.by.spendings.url,
                           budget.1912_15.by.spendings.codebook.url,
                           budget.1912_15.by.depart.url,
                           budget.1912_15.by.depart.codebook.url),
                  stringsAsFactors = FALSE)

# Not all codebooks can be loaded by the moment "2015-01-09 20:18:04 MSK"
for (i in 1:6) {
  if (!file.exists(file.path("data", src$files[i]))){
    try(download.file(src$urls[i], destfile = file.path("data", src$files[i])))
  }
}

# Data loading ------------------------------------------------------------

# Ordinary costs
budget.1912_15.by.depart <- tbl_df(read.csv(file.path("data", "budget.1912_15.by.depart.csv"),
                                            header = TRUE,
                                            sep = "\t",
                                            nrow = 59,
                                            stringsAsFactors = FALSE))

# Data preprocessing ------------------------------------------------------

budget.1912_15.by.depart %<>%
  # Switch to millions
  mutate(X1912_y = X1912_y/10^6,
         X1913_y = X1913_y/10^6,
         X1914_y = X1914_y/10^6,
         X1915_y = X1915_y/10^6)

# Plotting ----------------------------------------------------------------

budget.1912_15.by.depart %>%
  filter(budget.1912_15.by.depart$agency_code %% 100 == 0) %>%
  ggplot(aes(x = factor(agency_name, levels = rev(agency_name), ordered = T),
             y = X1912_y)) +
#     scale_y_continuous(trans = 'sqrt',
#                        breaks = trans_breaks('log10', function(x) 10^x),
#                        labels = trans_format('log10', math_format(10^.x))) +
    geom_bar(stat = "identity") +
    coord_flip() +
    xlab("department") +
    ylab("million rubles") +
    ggtitle("the Russian Empire budget by department, 1912-1915")

