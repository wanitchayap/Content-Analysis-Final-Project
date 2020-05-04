# Politeness Function of Emoji in CMCs
Wanitchaya Poonpatanapricha

## Introduction

Emoji, a form of ideograms, support computer-mediated communications (CMCs) in the same way nonverbal cues–such as facial expression, gesture, and tone of voice (Tang and Hew, 2019)–support face-to-face (FTF) communications. According to prior literature on emoticons (the predecessor of emoji), one way that emoji can support CMCs is to convey politeness. Since some languages have richer linguistic elements for politeness (e.g., honorifics in Japanese, sentence-final particles in Thai) than the others (e.g., English), it is possible that when a speaker whose primary language has richer elements for politeness needs to communicate politely through texts in foreign language with less rich elements for politeness, the speaker may resort to emoji due to the lack of linguistic elements for politeness as well as nonverbal cues to convey politeness as in FTF. This paper investigates such possibility by studying speakers with various primary languages communicating in English within an online community. The specific hypotheses are:  

1. A speaker, regardless of one’s primary language, is more likely to use emoji when communicates politely than when does not. (This hypothesis serves a purpose of replicating prior results on politeness function of emoticons on emoji.)  
2. A speaker whose primary language has richer elements for politeness is more likely to use emoji than a speaker whose primary language has less rich elements for politeness when communicates politely in English.

## Related Works

A number of research have linked the use of emoticons–the predecessor of emoji–with politeness strategies.  

Sampietro (2016) proposed that one of the pragmatic function of emoticons was to mitigate possible face-threats. For example, emoticons were used with requests and orders to soften these speech acts (Dresner and Herring, 2010; Darics, 2012; Skovholt et al., 2014). Specifically, Darics (2012) found that emoticons were mainly used to mitigate or to clarify the message, usually to reach a successful cooperation. Here are two example sentences mitigated by emoticons from Dresner and Herring (2010):

> I would like a noncircumventing solution ;->

> I wonder if you could recommend me some good readings related to conversational data. We just collected some IM data and are about to conduct some analysis on it. Since I’ve never worked on this kind of data before, I am writing for some suggestions.:)

Furthermore, Sampietro (2019) proposed that emoticons may contribute to politeness in CMCs. Specifically, Skovholt et al. (2014) found emoticons to be positive politeness markers and rapport building devices, and Vandergriff (2013) found emoticons to be mostly used in the service of politeness and to mitigate disagreement.

One of this paper’s goal is to replicate these prior results on politeness function of emoticons on emoji.

In terms of cross-language analysis, prior works have studied emoticon usages for politeness across different languages. Komrsková (2015) studied emoticons in Czech and English, and found that phrases of greeting and thanks were very often accompanied by emoticons in both languages. Kavanagh (2016) studied emoticon as a medium for channeling politeness within American and Japanese online blogging communities and found that Japanese used emoticons significantly more than Americans.

In contrast to these works which investigated the use of emoticons for politeness in each speaker’s primary language, this paper investigates the use of emoji for politeness in a speaker, regardless of the speaker’s primary language, communicating in English.

## Method
The goals of this paper are to replicate prior results on politeness function of emoticons on emoji and to investigate whether a speaker whose primary language has richer elements for politeness is more likely to use emoji than a speaker whose primary language less rich elements for politeness when communicates politely in English. To do so, an ideal dataset would be an English CMC that is a product of speakers with different primary languages where each speaker’s primary language as well as the intention to communicate politely are explicitly coded.

In this paper, the next best ideal data is used because it is not feasible to conduct a controlled experiment. In particular, the ideal dataset is approximated from an English CMC where some speakers’ preferred languages other than English are known. The intention to communicate politely is approximated from how polite the utterance is as well as whether that utterance is a direct mention to another speaker. Lastly, each language’s degree of richness in polite elements is approximated from the politeness distinctions in pronouns feature from WALS (Dryer and Haspelmath, 2013).

In terms of statistical test, logistic regression is used in a binary classification task predicting whether an utterance contain any emoji. Independent variables that are inputs of the logistic regression are a speaker’s primary language’s degree of richness in polite elements, how polite the utterance is, and whether the utterance is a direct mention to another speaker. In addition, according to Guntuku et al. (2019), there are some cultural differences in the use of emoji between the East and the West. Hence, a binary feature indicating whether the speaker’s primary language is from the East or the West is used as a control variable in the logistic regression.

### Data

The data of an English CMC was obtained from the official Discord server of Tsuki Adventure, a free-to-play mobile game. Discord is a proprietary freeware VoIP application and digital distribution platform designed for video gaming communities that specializes in text, image, video and audio communication between users in a chat.

What is special about Tsuki Adventure Discord is that each user in the community is asked to tag the user profile with what language other than English the user prefers to communicate in. Although the preferred language tag is not mandatory, there is a significant number of the users in this community that do so. Hence, there is sufficient data to investigate the research questions. Nevertheless, using this data requires a strong assumption that the language tagged is the primary language of that user. If this assumption is not true, the results from testing the second hypothesis should be more conservative. Hence, this assumption should make it harder to get significant results. Also note that testing the 4 first hypothesis doesn’t require the user’s primary language, while testing the second hypothesis requires one. Therefore, all utterances are used in testing the first hypothesis, while only utterances from users with language tags are used in testing the second hypothesis.

In term of the data collection, the data was scraped from the official discord server of Tsuki Adventure on 02/21/2020 using a Python Discord scraper from Dracovian (2019). The total number of scraped utterances was 3,371, of which 1,622 utterances came from users with language tags. There were 12 different languages being tagged: 7 are Eastern languages (Bahasa, Chinese, Japanese, Korean, Tagalog, Thai, and Vietnamese) and 5 are Western languages (French, German, Portuguese, Russian, and Spanish).

### Approximating a language’s degree of richness in polite elements via WALS’ politeness distinctions in pronouns

Politeness distinctions in pronouns feature from WALS (Dryer and Haspelmath, 2013)–The World Atlas of Language Structures–is used as a proxy for each language’s degree of richness in polite elements. This feature has been used in some prior social science research. For example, Davis and Abdurazokzoda (2016) studied relationship between politeness distinctions and egalitarianism, and found that this politeness distinctions feature from WALS was more reliable than the others.

The scope of this feature is restricted to politeness distinctions in second person pronouns. Below are the 4 categories of this politeness distinctions in pronouns feature with corresponding descriptions from WALS and Davis and Abdurazokzoda (2016):

1. No politeness distinctions - Languages that were assigned this value have no personal pronouns in their paradigms which are used to express different degrees of respect or intimacy toward the addressee. For example, there is no politeness distinction in English, in which “you” is used for both formal and familiar forms of address.  
2. Binary politeness distinctions - Languages that were assigned this value have a paradigmatic opposition between one intimate or familiar pronoun of address and another one expressing respectful address. The binary politeness distinction is found in many Indo-European languages, such as “du” and “Sie” in German or “tu” and “vous” in French.  
3. Multiple politeness distinctions - Languages that were assigned this value have two or more degrees of politeness within a pronominal paradigm. For example, In Marathi, “tu” is used for family members and friends, “te” and “he” are used for people with higher social status, and an extra polite form “apan” is used for priests and teachers and in very formal contexts. Note that these systems are rare cross-linguistically.  
4. Pronoun avoidance - The term "pronoun avoidance" describes a strategy of pronoun usage which has an effect on the overall shape of the paradigm. Languages of East and Southeast Asia such as Japanese, Burmese and Thai have a strong sensitivity to politeness in language usage and within their grammars. Speakers have to account for a variety of social distinctions linguistically. Social distinctions between speaker and hearer may reflect relative age, kinship, social ranking, intimacy, and other social features. From a linguistic point of view, one of the most important strategies of being polite is to avoid of addressing people directly.  

Since, in the data, there is no language besides English that has no politeness distinction and no preferred language tag for English, this category is omitted from the analysis. Also, since there is only Tagalog that is multiple politeness distinct in the data, using this category as it is in the analysis could lead to a scenario where the results from this category are due to Tagalog itself and not due to having multiple politeness distinctions. Thus, multiple politeness distinctions and binary politeness distinctions are combined into a single category in the analysis. The reason to combine multiple politeness distinctions with binary politeness distinctions instead of pronoun avoidance is because according to WALS and Davis and Abdurazokzoda (2016), the pronoun avoidance is an indication of politeness that goes beyond the use of formal pronouns. Hence, linguistically, multiple politeness distinctions are closer to binary politeness distinctions than to pronoun avoidance. In sum, there are 2 categories of politeness distinctions used in the analysis: binary/multiple politeness distinctions and pronoun avoidance.

### Approximating an intention to communicate politely via an utterance’s Polite Level and Direct Mention

#### Polite Level

One way to approximate the intention to communicate politely is to see how polite the produced utterance is. In order to quantify such quality, a maximum entropy classifier–which is commonly used in several text classifications–predicting an utterance’s polite level is trained from a human-annotated corpus created and used by Danescu-Niculescu-Mizil et al. (2013).
Danescu-Niculescu-Mizil et al. (2013)’s corpus has more than 10,000 utterances annotated by humans via Amazon Mechanical Turk (AMT) and these utterances came from two large online communities: Wikipedia and Stack Exchange. According to Danescu-Niculescu-Mizil et al. (2013), this corpus was thus far "the largest corpus with politeness annotations". Each utterance was labeled by five different annotators. The standard z-score normalization was applied to each worker’s scores to account for subjectivity, and finally, the politeness score of an utterance was defined as the average of the five normalized scores assigned by the annotators

To convert the corpus’ continuous politeness score into a categorical variable to be predicted by the maximum entropy classifier, the corpus is binned into 5 equal percentile width bins which are labeled as -2,-1,0,1, and 2 respectively (henceforth polite level). A maximum entropy classifier with the polite level as the target labels is then trained on a 80-20 train-test split of the corpus with the utterance’s bag-of-words as the input features.

The trained maximum entropy classifier is then applied on the current dataset to classify the polite level of each utterance. The input features are the bag-of-words of each utterance after removing any emoji contained. The polite level variable is treated as an ordinal categorical variable in the analysis.

#### Direct Mention

Since the polite level is measured automatically, there could be some cascading errors from the training corpus (which is the least likely given the reliability of the corpus), training the classifier, or applying the trained classifier on the current data. In addition, there could still be some aspect of politeness that the trained classifier fails to capture. Therefore, direct mention, which is defined as when an utterance contains Discord’s mentioning users feature to mention the other user(s), is used as another proxy of an intention to communicate politely that does not suffer from possible cascading errors like the polite level does. Note that direct mention is not equivalent to a private chat between users. Similar to other social media and online community platforms, utterances contain Discord’s mentioning users feature are publicly accessible to all users.

The logic behind using direct mention as a proxy of an intention to communicate politely comes from Brown and Levinson (1978)’s theory of politeness. The theory explains that all persons are concerned with their face and recognize that others also have face wants, and it is generally in everyone’s interests 8 to maintain each other’s face. The risk of failing to maintain the other’s face (or being not polite) is then highest when there is a particular other’s face to maintain. That is if a user produces a not so polite utterance without directly mentioning any other user, it is less clear whose face is not maintained and hence it is less likely that any other user’s face will not be maintained at all. On the other hand, if a user produces a not so polite utterance and directly mentions the other user(s), it is definite that the other user(s)’s face(s) will not be maintained. Hence, when there is a direct mention, the user should be more likely to communicate politely.

## References

   Penelope Brown and Stephen C Levinson. 1978. Universals in language usage: Politeness phenomena. In Questions and politeness: Strategies in social interaction, pages 56–311. Cambridge University Press.

   Cristian Danescu-Niculescu-Mizil, Moritz Sudhof, Dan Jurafsky, Jure Leskovec, and Christopher Potts. 2013. A computational approach to politeness with application to social factors. arXiv preprint arXiv:1306.6078.

   Erika Darics. 2012. Instant messaging in work-based virtual teams: the analysis of non-verbal communication used for the contextualisation of transactional and relational communicative goals. Ph.D. thesis, Loughborough University.

   Lewis S Davis and Farangis Abdurazokzoda. 2016. Language, culture and institutions: Evidence from a new linguistic dataset. Journal of Comparative Economics, 44(3):541–561.

   Dracovian. 2019. Dracovian/discord-scraper.

   Eli Dresner and Susan C Herring. 2010. Functions of the nonverbal in cmc: Emoticons and illocutionary force. Communication theory, 20(3):249–268.

   Matthew S. Dryer and Martin Haspelmath, editors. 2013. WALS Online. Max Planck Institute for Evolutionary Anthropology, Leipzig.

   Sharath Chandra Guntuku, Mingyang Li, Louis Tay, and Lyle H Ungar. 2019. Studying cultural differences in emoji usage across the east and the west. In Proceedings of the International AAAI Conference on Web and Social Media, volume 13, pages 226–235.

   Barry Kavanagh. 2016. Emoticons as a medium for channeling politeness within american and japanese online blogging communities. Language & Communication, 48:53–65.

   Zuzana Komrsková. 2015. The use of emoticons in polite phrases of greetings and thanks. International Journal of Social, Behavioral, Educational, Economic, Business and Industrial Engineering, 9(4):1309–1312.

   Agnese Sampietro. 2016. Exploring the punctuating effect of emoji in spanish whatsapp chats. Lenguas modernas, (47).

   Agnese Sampietro. 2019. Emoji and rapport management in spanish whatsapp chats. Journal of Pragmatics, 143:109–120.

   Karianne Skovholt, Anette Grønning, and Anne Kankaanranta. 2014. The communicative functions of emoticons in workplace e-mails::-. Journal of Computer-Mediated Communication, 19(4):780–797.

   Ying Tang and Khe Foon Hew. 2019. Emoticon, emoji, and sticker use in computer-mediated communication: A review of theories and research findings. International Journal of Communication, 13:27.

   Ilona Vandergriff. 2013. Emotive communication online: A contextual analysis of computer-mediated communication (cmc) cues. Journal of Pragmatics, 51:1–12.
