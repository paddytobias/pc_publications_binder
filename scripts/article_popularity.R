query = 'select  article_to_search.article_doi, search_name, readers_count
from altmetrics, article_to_search
where altmetrics.article_doi=article_to_search.article_doi'
readers = dbGetQuery(con, query)

readers[3:ncol(readers)] = sapply(readers[3:ncol(readers)],as.numeric)
readers_gather = readers %>% 
  gather(key = type, value = readers_count, -c(article_doi, search_name)) %>% 
  group_by(article_doi) %>% 
  summarise(readers_sum_count = sum(readers_count, na.rm = T))

query = 'select  article_to_search.article_doi, search_name, views
from altmetrics, article_to_search
where altmetrics.article_doi=article_to_search.article_doi'
views = dbGetQuery(con, query)
views_gather = views %>% 
  gather(key = type, value = views_count, -c(article_doi, search_name)) %>% 
  group_by(article_doi) %>% 
  summarise(views_sum_count = sum(views_count, na.rm = T))

query = 'select  article_to_search.article_doi, search_name, citations,"cohorts.doc", "cohorts.sci", "cohorts.doc", "cohorts.sci", cited_by_peer_review_sites_count, cited_by_book_reviews_count, cited_by_policies_count
from altmetrics, article_to_search
where altmetrics.article_doi=article_to_search.article_doi'
citations = dbGetQuery(con, query)
citations[3:ncol(citations)] = sapply(citations[3:ncol(citations)],as.numeric)

citations_gather = citations %>% 
  gather(key = type, value = citations_count, -c(article_doi, search_name)) %>%
  group_by(article_doi) %>% 
  summarise(citations_sum_count = sum(citations_count, na.rm = T))

query = "select  article_to_search.article_doi, search_name, 
cited_by_fbwalls_count, cited_by_gplus_count, cited_by_rdts_count, cited_by_wikipedia_count, 
cited_by_msm_count, cited_by_feeds_count
from altmetrics, article_to_search
where altmetrics.article_doi=article_to_search.article_doi"
mentions = dbGetQuery(con, query)
mentions[3:ncol(mentions)] = sapply(mentions[3:ncol(mentions)],as.numeric)
mentions_gather = mentions %>% 
  gather(key = type, value = mentions_count, -c(article_doi, search_name)) %>% 
  group_by(article_doi) %>% 
  summarise(mentions_sum_count = sum(mentions_count, na.rm = T))

## aggregations of Pop
article_popularity = readers_gather %>% 
  left_join(views_gather, by = c("article_doi" = "article_doi")) %>% 
  left_join(citations_gather, by = c("article_doi" = "article_doi")) %>% 
  left_join(mentions_gather, by = c("article_doi" = "article_doi")) %>% 
  select(article_doi, ends_with("count"), ends_with("_z")) %>% 
  mutate(exposure = readers_sum_count+views_sum_count+citations_sum_count+mentions_sum_count) %>% 
  rename(mentions = mentions_sum_count,
         readership = readers_sum_count,
         citations = citations_sum_count, 
         views = views_sum_count)

# removing extreme outlier here: https://www.tandfonline.com/doi/abs/10.1080/14678802.2012.703531
article_popularity = article_popularity %>% 
  filter(exposure<500000)

ggplot(article_popularity, aes(sample = exposure))+
  geom_qq()

## enhance Pop with search name and year
pub_by_year_search_pc = pub_by_year_search %>% 
  filter(search_name %in% pc_cats)


pop_year_search = article_popularity %>% 
  inner_join(pub_by_year_search_pc) %>% filter(!(search_name %in% meta_cats)) %>% 
  filter(!(search_name %in% meta_cats)) %>% 
  select(article_doi, search_name, year, everything()) 

pop_year_search$search_name = factor(pop_year_search$search_name, levels = reorder_pc)
bibmets_year_search = pop_year_search %>%
  #add_count(search_name, year) %>% 
  group_by(search_name, year) %>% 
  summarise(#pop_sum = sum(popularity),
            readers = sum(readership), 
            mentions = sum(mentions),
            citations = sum(citations),
            views = sum(views),
            total_exposure = sum(exposure),
            pub_count = n()) %>% 
  filter(#year > 2000 &year < 2018, 
         search_name %in% pc_cats) 

write_csv(bibmets_year_search, "output_data/bibmets_year_search.csv")
bibmets_year_search_gather = bibmets_year_search %>% 
  gather(type, value, c(readers, mentions, citations))

p = ggplot(bibmets_year_search_gather, aes(pub_count, value,colour=type, 
                                           text=sprintf("%s<br>%s: %s<br>Pubs: %s", 
                                                        year, type, value, pub_count)))+
  geom_point(alpha=0.7, size=2.5)+
  theme_minimal()+
  labs(y="Engagement", x = "Publication", colour="Engagement", title="Engagement stats overtime")

ggplotly(p, tooltip="text")

p = ggplot(bibmets_year_search_gather, aes(year, value,colour=type, 
                                           text=sprintf("%s<br>%s: %s<br>Pubs: %s", 
                                                        year, type, value, pub_count)))+
  geom_jitter(alpha=0.7, size=2.5)+
  scale_y_log10()+
  theme_minimal()+
  labs(y="Engagement", x = "Year", colour="Engagement", title="Engagement stats overtime")

ggplotly(p, tooltip="text")



lm(bibmets_year_search$pub_count~bibmets_year_search$mentions, bibmets_year_search)
cor(bibmets_year_search$pub_count,bibmets_year_search$mentions)
lm(bibmets_year_search$pub_count~bibmets_year_search$citations, bibmets_year_search)
cor(bibmets_year_search$pub_count,bibmets_year_search$citations)
lm(bibmets_year_search$pub_count~bibmets_year_search$readers, bibmets_year_search)
cor(bibmets_year_search$pub_count,bibmets_year_search$readers)

correls = data.frame(`Engagement types`=c("Mentions", "Citations", "Readers"), 
                     `To Pub count correlations`=c(cor(bibmets_year_search$pub_count,bibmets_year_search$mentions), 
                                       cor(bibmets_year_search$pub_count,bibmets_year_search$citations),
                                       cor(bibmets_year_search$pub_count,bibmets_year_search$readers)))


p = ggplot(bibmets_year_search_gather, aes(pub_count, value,colour=type, 
                                           text=sprintf("%s<br>%s: %s<br>Pubs: %s", 
                                                        year, type, value, pub_count)))+
  geom_point(alpha=0.7, size=2.5)+
  theme_minimal()+
  labs(x="Citations, Mentions & Readership counts", y = "Publication count", colour="Engagement")+
  facet_wrap(~search_name, scales = "free")

ggplotly(p, tooltip="text")

