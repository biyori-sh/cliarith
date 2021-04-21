# CLiArith
_NOTE: CLiarith is primarily for my practice._

CLiArith is a simple _trial_ implementation of the interval arithmetic in Common Lisp based on the simple implementation coded by C/C++ ([Simple Interval Arithmeric Library](http://verifiedby.me/kv/simple/index-e.html)).

_CLiArith is available only in SB CL._

- [mskashi/kv Github](https://github.com/mskashi/kv)
- [Marco Antoniotti, Why You Cannot (Yet) Write an "Interval Arithmetic" Library in Common Lisp](https://arxiv.org/abs/2003.03831)


## Usage
To be described.


## Examples
Set `*read-default-float-format*` to `(fp-type)-float`, and bind the values of `(fp-type)-float-epsilon` and `(fp-type)-float-negative-epsilon` to `*positive-epsilon` and `*negative-epsilon` respectively
```Lisp
(cliarith:set-float-format 'double-float)
```

Addition:
```Lisp
(cliarith:+[] 0.1 0.1 0.1 0.1 0.1)
;; => #S(CLIARITH:[] :LOW 0.49999999999999994 :HIGH 0.5000000000000001)
```
Subtraction:
```Lisp
(cliarith:-[] 0.1 0.1 0.1 0.1 0.1)
;; => #S(CLIARITH:[] :LOW -0.30000000000000004 :HIGH -0.3)
```
Multiplication:
```Lisp
(cliarith:*[] 0.1 0.1 0.1 0.1 0.1)
;; => #S(CLIARITH:[] :LOW 1.0e-5 :HIGH 1.0000000000000008e-5)
```
Division:
```Lisp
(cliarith:/[] 0.1 0.1 0.1 0.1 0.1)
;; => #S(CLIARITH:[] :LOW 1000.0 :HIGH 1000.0000000000005)
```

Extend a point and an interval with `*negative-epsilon` and `*positive-epsilon`
```Lisp
(cliarith:extend[] 1.0)
;; => #S(CLIARITH:[] :LOW 0.9999999999999999 :HIGH 1.0000000000000002)
```

## Installation
Put this repository, for instance, got by `git clone` in your ASDF path and load it:
```
(asdf:load-system :cliarith)
```
or, if you use `roswell`, install this repository with
```
ros install biyori-sh/cliarith
```
and load it:
```
(ql:quickload :cliarith)
```
