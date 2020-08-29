# Analysis of Changing Stances on Coal Power in Political Speeches

## Project Goal

For the seminar "Solving the Climate Crisis with Text Mining" at the Hasso Plattner Institute Potsdam we developed this 
project with the goal of identifying the policy positions of German political parties on the topic of coal power. We are
using a dataset of speeches from the German Bundestag going back to 1949, all of them containing some mention of a word 
with the substring "Kohle".
 
## Project Structure
 
The project is separated into two main approaches, [Wordfish](http://www.wordfish.org/) and 
[Wordscores](https://www.tcd.ie/Political_Science/wordscores/index.html). For both approaches we provide an example
usage file in the ``src`` directory. The approach implementation as well as utility functions can be found in the
respective ``src/wordfish`` or ``src/wordscores`` directories. General utility functions are to be found in the 
``src/utils`` directory.

We also looked into running traditional, dictionary-based sentiment analysis on the data, but found it to not be 
applicable to this problem. The code for this attempt can be found in ``src/sentiment_analysis``.

The ``sandbox.R`` file was only used for development and testing, so it is not up-to-date and should thus not be used as
an entry point to our project code.