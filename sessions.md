Sessions in enTangleD
=====================

Sessions are series of actions that you'd normally do in a REPL, and are piped through some executable. Examples are scripts in Bash, Python or GnuPlot. The contents of the session act as the `stdin` of a given command. The output of such a command can be inserted into the PanDoc data stream. Alternatively we may state that the command generates some output file.

Leading in how we setup such a feature should be: what is the narrative?

* Notebook style: We may be explaining some function behaviour, giving output every few commands, Jupyter notebook style. This may be the trickiest thing to implement. If we take Org-mode behaviour as an example here, we may think of having the daemon update results on-save. If scripts are fast enough we can renew every run, but often we may want to cache intermediate results, opening a huge can of worms.
* Plotting:


We will run this command in Python

``` {.python .workflow #my-awesome-script}
print("Hello")
```

forwarding this result to the following ruby script...

``` {.ruby .workflow depends=my-awesome-script}
```
