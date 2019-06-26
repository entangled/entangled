# Markdown

Markdown is widely used for writing up documents. It is supported by many blog engines, github readme, you name it. However, there are many varieties of markdown. One place where many flavours of markdown meet is in Pandoc. Pandoc can convert markdown to many different formats like HTML, LaTeX, PDF (through LaTeX), RTF, DOC etc. There are some little known features of markdown that make it very versatile and applicable to any application needing some form of rich content management.

## CSS attributes

Any element in a markdown document can be adorned with CSS attributes by appending them in curly braces: `{.class #id attr=value}`. This also applies to code blocks: `\`\`\`python` is equivalent to `\`\`\` {.python}`.

## DIV elements

A `div` element can be added using three colons.

~~~
::: {.warning}
This is a warning!
:::
~~~

This can be used to write down any non-standard element. So how is this rendered? This is where pandoc comes in. Pandoc has support for filtering elements and creating relevant output.


