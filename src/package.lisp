(defpackage cliarith
  (:use :cl)
  (:export :round-to
           :round-positive
           :round-negative
           :round-nearest
           :[])
  (:export :pointp
           :wd[]p
           :elementp
           :sub[]p
           :zero[]p
           :positive[]p
           :negative[]p
           :=[])
  (:export :wd[])
  (:export :diameter
           :radius
           :center
           :point->[]
           :abs[]-min
           :abs[]-max
           :abs[]
           :+[]
           :incf[]
           :incf[]id
           :-[]
           :decf[]
           :decf[]id
           :*[]
           :/[]))
