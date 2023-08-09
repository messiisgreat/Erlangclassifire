## <a name='overview'>Overview</a>

Classifier is a bayesian text analyzer and classifier. Its goal is provide a simple way to decide if a given text is considered legit or spam, based on a sample pool of texts you provide to the program.

Its logic is based upon Paul Graham's [A Plan For Spam](http://www.paulgraham.com/spam.html "A Plan For Spam").

## <a name='Contact Us'>Contact Us</a>

For **questions** or **general comments** regarding the use of this library, please use our public
[hipchat room](https://www.hipchat.com/gpBpW3SsT).

If you find any **bugs** or have a **problem** while using this library, please [open an issue](https://github.com/inaka/classifier/issues/new) in this repo (or a pull request :)).

And you can check all of our open-source projects at [inaka.github.io](http://inaka.github.io)

## <a name='features'>Features</a>

- You can tweak the parameters.
- You can manually flag spams and false positives.
- The program learns overtime from new texts and updates the sample pools.

## <a name='configuration'>Configuration</a>
  you need to define classifier as a rebar dep or have some other way of including it in erlang’s path.

  To configure classifier you use an application variable (probably in your app.config):

```  
{classifier, [  
  {update_probabilities_timeout, 300000}, %% milliseconds  
  {default_probability, 0.4},  
  {threshold_probability, 0.9},  
  {max_text_tokens, 5},  
  {minimun_appearances, 5}  
]} 
```

  All the config params have a default value, so you can skip some or all of them in your config 

## <a name='usage'>Usage</a>
  First of all you need to __start the app__:

  ```
  application:start(classifier)
  ```
  The next step is training the classifier. You can train it whenever you want and as many times as you want. You need to train it before the first time you start using your app.  
  There're three ways to __train it__:  
  * Passing a __dir__ 
     
  ```
  classifier:train(Dir)
  ```  
  
  Where Dir is a path to some folder that contains two folders called pos and neg where there're files with texts to be analyzed.
  You can find an example in priv/test dir.
  * Passing a __text__
     
      ```
      classifier:train({Tag, text, Text})
      ```  
  
      Where Tag is 'pos' or 'neg' and Text is a string to be putted on the Tag side.
  * Passing a __text list__
     
      ```
      classifier:train({Tag, text_list, Texts})
      ``` 
  Where Tag is 'pos' or 'neg' and Texts is a list of strings to be pushed on the Tag side.
  
Now you can ask the classifier to analyze and classify some text:
  ```
  1> classifier:classify(Text).
  acceptable
  2> classifier:classify(AnotherText).
  unacceptable
  ```
Every time the classifier classify a text it learns about the result pushing the text analyzed on its pool
  
## <a name='features'>Future Features</a>
  - Persist the info
  - Multiprocess to classify text and to update the state
