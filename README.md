# CLiArith
_NOTE: CLiarith is primarily my practice._

CLiArith is a simple _trial_ implementation of the interval arithmetic in Common Lisp based on the simple implementation coded by C/C++ ([Simple Interval Arithmeric Library](http://verifiedby.me/kv/simple/index-e.html)).

_CLiArith is available only in SBCL._

- [mskashi/kv Github](https://github.com/mskashi/kv)
- [Marco Antoniotti, Why You Cannot (Yet) Write an "Interval Arithmetic" Library in Common Lisp](https://arxiv.org/abs/2003.03831)


## Usage
To be described.

Addition:
```Lisp
(let ((result (cliarith:[] 0.0 0.0)))
  (dotimes (i 100 result)
    (setf result (cliarith:+[] 0.1 result))))
;; => #S(CLIARITH:[] :LOW 9.999982 :HIGH 10.000027)
```

Multiplication:
```Lisp
(let ((result (cliarith:[] 1.0 1.0)))
        (dotimes (i 100 result)
          (setf result (cliarith:*[] (cliarith:[] 0.95 0.95)  result))))
;; => #S(CLIARITH:[] :LOW 0.0059204977 :HIGH 0.00592055)
```

Subtraction`cliarith:-[]` and division`cliarith:/[]` are implemented.


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
