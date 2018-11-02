source("../scripts/libraries.R")
library(htmlwidgets)
source("../../peacebuilding-language/database/db_connect.R")
source("../scripts/globals.R")

# query = "SELECT a.article_doi AS article_doi, 
# a.year AS year, 
# s.search_name AS search_name
# FROM articles a, article_to_search s
# WHERE a.article_doi = s.article_doi"
# pub_by_year_search = dbGetQuery(con, query)
# 
# pub_count_year_search = pub_by_year_search %>% 
#   count(year, search_name) %>% 
#   filter(search_name %in% pc_cats) %>% 
#   mutate(cats = ifelse(search_name %in% peace_cats, "peace-oriented", "conflict-oriented"), 
#          stages = ifelse(search_name %in% long_pc, "long-term", 
#                          ifelse(search_name %in% middle_pc, "medium-term", "short-term"))) %>% 
#   filter(!is.na(year))
# 
# write_csv(pub_count_year_search, "output_data/pub_count_year_search.csv")

pub_count_year_search = read_csv("../data/pub_count_year_search.csv")

pub_count_year_search_summary = pub_count_year_search %>% 
  group_by(search_name, year) %>%
  summarise(sum = sum(n)) #%>% 
#filter(year > input$years[1] & year < input$years[2]) %>% 
#filter(search_name %in% input$search_nam

p = ggplot(pub_count_year_search_summary, aes(year, sum))+
  geom_line(aes(color = search_name))+
  theme_minimal()+
  labs(y="Publication count", x = NULL, color = NULL)

dir.create("../output_viz/")
setwd("../output_viz/")
saveWidget(ggplotly(p), file = "pub_count_year_search.html")
setwd("../Rproj/")

pub_count_year_search_summary_spread = pub_count_year_search_summary %>% 
  spread(key = year, value = sum, fill = 0)

aov = lm(pub_count_year_search$n ~ pub_count_year_search$search_name, data = pub_count_year_search)
a = anova(aov)
a$`Pr(>F)`[1]


TukeyHSD(aov(pub_count_year_search$n ~ pub_count_year_search$search_name),which = 'pub_count_year_search$search_name', conf.level=0.95 )

# with PB removed
# pub_count_year_search_no_pb = pub_count_year_search %>% 
#   filter(search_name!="peacebuilding")
# aov = lm(pub_count_year_search_no_pb$n ~ pub_count_year_search_no_pb$search_name, data = pub_count_year_search_no_pb)
# anova(aov)
# TukeyHSD(aov(pub_count_year_search_no_pb$n ~ pub_count_year_search_no_pb$search_name),which = 'pub_count_year_search_no_pb$search_name', conf.level=0.95 )
# 

max_median_pub_count = pub_count_year_search %>% 
  group_by(search_name) %>% 
  summarise(median = median(n)) %>% 
  ungroup() %>% 
  filter(median == max(median)) %>% 
  select(search_name)


pub_count_bp_data = pub_count_year_search %>% 
  mutate(is.peacebuilding = ifelse(search_name==as.character(max_median_pub_count$search_name), TRUE, FALSE))

ggplot(pub_count_bp_data, aes(search_name, n, fill = is.peacebuilding))+
  geom_boxplot(show.legend = F)+
  labs(x= "Search terms", y="Yearly counts", title = paste(min(pub_count_bp_data$year), "to", max(pub_count_bp_data$year)))+
  theme_minimal()

pub_count_year_search %>% 
  group_by(search_name) %>% 
  #mutate(Year.max=max(n)) %>% 
  summarise(Average=mean(n),
            Median=median(n),
            `StdDev`=sd(n), 
            Min=min(n), 
            Max=max(n)) %>% 
  arrange(desc(Average)) %>% 
  inner_join(pub_count_year_search, by = c("search_name"="search_name","Max"="n")) %>% 
  select(-c("cats", "stages")) %>% 
  rename(Max.year=year)

##############

query = "SELECT a.article_doi, a.title, a.abstract, a.year, s.search_name 
FROM articles a, article_to_search s 
WHERE a.article_doi=s.article_doi"
abs_text_db = dbGetQuery(con, query)

search_names = unique(abs_text_db$search_name)
abs_text_db = abs_text_db[abs_text_db$abstract!="",]

abs_text = abs_text_db %>% 
  filter(year > 2000) %>% 
  group_by(search_name, year) %>% 
  sample_n(1000, replace=TRUE)

abstracts_tokenised_clean = abs_text %>%
  unnest_tokens(word, abstract) %>%
  anti_join(stop_words) %>% 
  mutate(word = removeNumbers(word)) %>% 
  filter(!(word %in% other_stop_words))

write_csv(abstracts_tokenised_clean, "output_data/abstracts_tokenised_clean.csv")

