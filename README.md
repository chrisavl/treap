# [Treap](http://en.wikipedia.org/wiki/Treap) data structure for Erlang

This is an implementation of a Treap for Erlang, a data structure for key-value pairs.

The treap module lets you split and merge treaps efficiently. It also lets you set the priority of a key in the treap manually, to speed up access to commonly accessed keys. Although in most cases gb\_trees/dict should be faster than the treap module.

You can benchmark it versus other common data structures by running:

```
$ wget http://learnyousomeerlang.com/static/erlang/keyval_benchmark.erl && patch < benchmark.patch 
$ erl -pa ebin
> c(keyval_benchmark).
> keyval_benchmark:bench(100000).
```


### Usage

Pretty much the same as the dict module, check the source for exports/functions. It is < 200 lines :)
