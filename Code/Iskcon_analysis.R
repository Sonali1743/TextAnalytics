library(tidyverse)
library(tidytext)
library(SnowballC)
library(deeplr)

iskcon_data = read_csv("~/Desktop/Okstate/BAN 5743/Week11/translated_topics.csv")

#Tokenization
iskcon_text = select(iskcon_data, text_translated)
tidy_dataset = unnest_tokens(iskcon_text, word, text_translated)

#Removing stop words
data("stop_words")
tidy_dataset1 = anti_join(tidy_dataset, stop_words)
counts = count(tidy_dataset1, word)
arrange(counts, desc(n)) %>%
  ungroup %>%
  slice(1:15)

#Removing numbers
patterndigits = '\\b[0-9]+\\b'
tidy_dataset1$word = str_remove_all(tidy_dataset1$word, patterndigits)
counts2 = count(tidy_dataset1, word)
arrange(counts2, desc(n)) %>%
  ungroup %>%
  slice(1:15)

#Removing lines, tabs, and blank spaces
tidy_dataset1$word = str_replace_all(tidy_dataset1$word, '[:space:]', '')
tidy_dataset1 = filter(tidy_dataset1,!(word == ''))
counts3 = count(tidy_dataset1, word)
arrange(counts3, desc(n)) %>%
  ungroup %>%
  slice(1:15)

#Stemming
stemmed_data = wordStem(tidy_dataset1$word, language="en")
tidy_dataset2 = mutate_at(tidy_dataset1, "word", funs(wordStem((.), language="en")))
counts4 = count(tidy_dataset2, word)
arrange(counts4, desc(n)) %>%
  ungroup %>%
  slice(1:15)

#Visualization of tokens frequency
frequency = tidy_dataset2 %>%
  count(word) %>%
  arrange(desc(n)) %>%
  mutate(proportion = (n / sum(n)*100)) %>%
  filter(proportion >= 0.5)
library(scales)
ggplot(frequency, aes(x = proportion, y = word)) +
  ggtitle("Words Frequency") +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  theme(legend.position="none") +
  labs(y = 'Word', x = 'Proportion')

library(wordcloud)
library(udpipe)
library(lattice)

#Emotion analysis
#1. Joy and Sadness
nrc_joysad = get_sentiments('nrc') %>%
  filter(sentiment == 'joy' | 
           sentiment == 'sadness')
newjoin_js = inner_join(tidy_dataset2, nrc_joysad)
counts_js = count(newjoin_js, word, sentiment)
spread_js = spread(counts_js, sentiment, n, fill = 0)

text_cont = spread_js %>%
  mutate(cont = joy-sadness, overall_cont = case_when(cont>0~'joy',
                                                      cont==0~'neutral',
                                                      cont<0~'sadness'))

text_cont = arrange(text_cont, desc(cont))
text_joysad = text_cont %>%
  slice(1:15,196:210)

ggplot(text_joysad, aes(x=word, y=cont, fill = overall_cont)) +
  ggtitle("Frequency of Words Expressing Contentment") +
  coord_flip() +
  theme_light(base_size = 10) +
  labs(
    x='Word',
    y='Contentment'
  ) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  ) +
  geom_col()

#2. Trust and anger
nrc_truang = get_sentiments('nrc') %>%
  filter(sentiment == 'trust' | 
           sentiment == 'anger')
newjoin_ta = inner_join(tidy_dataset2, nrc_truang)
counts_ta = count(newjoin_ta, word, sentiment)
spread_ta = spread(counts_ta, sentiment, n, fill = 0)

text_feel = spread_ta %>%
  mutate(feel = trust-anger, overall_feel = case_when(feel>0~'trust',
                                                      feel==0~'neutral',
                                                      feel<0~'anger'))

text_feel = arrange(text_feel, desc(feel))
text_feel2 = text_feel %>%
  slice(1:15,253:267)

ggplot(text_feel2, aes(x=word, y=feel, fill = overall_feel)) +
  ggtitle("Frequency of Words Expressing Feeling") +
  coord_flip() +
  theme_light(base_size = 10) +
  labs(
    x='Word',
    y='Feeling'
  ) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  ) +
  geom_col()

#3. Positive and negative

bing = get_sentiments('bing')

text_posneg = inner_join(tidy_dataset2,bing)
count_pn = count(text_posneg,word, sentiment)
spread_pn = spread(count_pn, sentiment, n, fill = 0)

text_satis = spread_pn %>%
  mutate(satisfaction = positive-negative, overall_satis = case_when(satisfaction>0~'positive',
                                                                     satisfaction==0~'neutral',
                                                                     satisfaction<0~'negative'))
text_satis = arrange(text_satis, desc(satisfaction))

text_satis2 = text_satis %>%
  slice(1:15,436:450)

ggplot(text_satis2, aes(x=word, y=satisfaction, fill = overall_satis)) +
  coord_flip() +
  theme_light(base_size = 15) +
  labs(
    x='Word',
    y='Satisfaction'
  ) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  ) +
  geom_col()

library(reshape2)
tidy_dataset2 %>%
  inner_join(bing) %>%
  count(word, sentiment) %>%
  slice(1:40,410:450) %>%
  acast(word~sentiment, value.var='n',fill=0) %>%
  comparison.cloud(colors=c('gray30','gray70'))

#POST

library(udpipe)

ud_model = udpipe_download_model(language = "english")
tidy_post1 = tidy_dataset2 %>% 
  select(word)
ud_model = udpipe_load_model(ud_model$file_model)
tagging_data = as.data.frame(udpipe_annotate(ud_model, x = tidy_post1$word))

post_stats = txt_freq(tagging_data$upos)
post_stats$key = factor(post_stats$key, levels = rev(post_stats$key))

#Nouns
noun_stats = subset(tagging_data, upos %in% c("NOUN"))
noun_stats2 = txt_freq(noun_stats$token)

noun_stats2$key = factor(noun_stats2$key, levels = rev(noun_stats2$key))
noun_stats2 %>%
  slice(1:20) %>%
  ggplot(aes(x=key, y=as.factor(freq), fill=freq)) +
  coord_flip() +
  theme_light(base_size = 15) +
  labs(
    x='Frequency',
    y='',
    title='Noun Occurrences'
  ) +
  theme(
    legend.position = 'none',
    panel.grid = element_blank(),
    axis.title = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    title = element_text(size = 13)
  ) +
  scale_fill_gradient(low="orange", high="orange3") +
  geom_col()

#Adjectives
adjstats = subset(tagging_data, upos %in% c("ADJ"))
adjstats2 = txt_freq(adjstats$token)
adjstats2$key = factor(adjstats2$key, levels = rev(adjstats2$key))
adjstats2 %>%
  slice(1:20) %>%
  ggplot(aes(x=key, y=as.factor(freq), fill=freq)) +
  coord_flip() +
  theme_light(base_size = 15) +
  labs(
    x='Frequency',
    y='',
    title='Adjective Occurrences'
  ) +
  theme(
    legend.position = 'none',
    panel.grid = element_blank(),
    axis.title = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    title = element_text(size = 13)
  ) +
  scale_fill_gradient(low="chartreuse", high="chartreuse3") +
  geom_col()

#Verbs
verbstats = subset(tagging_data, upos %in% c("VERB"))
verbstats2 = txt_freq(verbstats$token)
verbstats2$key = factor(verbstats2$key, levels = rev(verbstats2$key))
verbstats2 %>%
  slice(1:20) %>%
  ggplot(aes(x=key, y=as.factor(freq), fill=freq)) +
  coord_flip() +
  theme_light(base_size = 15) +
  labs(
    x='Frequency',
    y='',
    title='Verb Occurrences'
  ) +
  theme(
    legend.position = 'none',
    panel.grid = element_blank(),
    axis.title = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    title = element_text(size = 13)
  ) +
  scale_fill_gradient(low="tan", high="tan3") +
  geom_col()



