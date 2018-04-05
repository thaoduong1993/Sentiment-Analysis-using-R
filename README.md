# Sentiment-Analysis-using-R
Exploratory sentiment analysis on the reviews of Toyota Camry (https://www.cars.com/research/toyota-camry/)

What did I learn???
1. Use a separate source code file for custom function
Again, this time I used Shiny App to visualize the results. But I learned to create custom function on a seperate R source code so I can run them for multiple output, instead of having to repeate each function for every output rendering.

2. Make use of %>%
This is the greatest discovery for me, an R newbie. It made writing long codes much shorter, easier, and fancier. It's easier to follow the flow of multiple functions through these pipes than going backwards when multiple function are nested.

3. Retrieve online data with HTML/CSS code
It was a bit tricky at the beginning when Selector Gadget did not work well with www.cars.com, so it took me a while to get the write css class to includ in rvest html_nodes and html_attributes function.

4. Simple cleaning technique for text data
I used stringr and tm to normalize the text for each review as well as classify each review to a category that the user mention (like when you're buying a car, you probably want to read about "price", "handling", "interior", etc.

5. Try different sentiment score method: Afinn, Bing, NRC
So far, my favorite part is NRC, because it includes different types of sentiment, from positive, negative, to fear, anger, joy, ... I think this multi-category sentiment can be apply for different kind of analytics, i.e. movie reviews, concert reviews, restaurants ....

6. TF-IDF is a great way to pick most common "improtant" terms
TF-IDF (Term Frequency - Inverse Document Frequency) not only counts how many times a word appears in a document (review), but also penalizing it if it appears too often in a greater context (i.e. service with "handling" tag). TF-IDF helps remove most of unimportant word (besides stop-word that you can easily remove with a function). Guess what are the top ten words for "price" tag :)

7. Basic rocks!
What I realize when trying to plug myself in this project is it's important to have a solid knowledge of different types of data, matrix, input, etc. so I don't run into too many errors when trying to a code. Of course, I solved it by Googling, but my goal is go back to Advandce R by Wickham to re-learn the basics. I doubt this would be much more exciting that the first time I read it (and left haft way).

8. What's on my mind?
How correctly I can "read" one's sentiment? When comparing the average (after scaling) of the star rating and the sentiment scores, there was a big gap. Sentiment analysis is a very complex subject, and I probbaly need to get my hands dirty on n-gram in order to get better score at sentiment. But till there, I'm glad that I finished this project and got a decent result.
