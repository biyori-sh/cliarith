(defpackage cliarith
  (:use :cl)
  (:export :*default-rounding-mode*
           :round-to
           :round-positive
           :round-negative
           :round-nearest
           :[]
           :get-epsilon
           :*positive-epsilon
           :*negative-epsilon
           :set-float-format)
  (:export :pointp
           :valid[]p
           :element[]p
           :sub[]p
           :zero[]p
           :positive[]p
           :negative[]p
           :=[]
           :intersection[]p)
  (:export :valid[])
  (:export :diameter
           :radius
           :center
           :->[]
           :extend[]
           :sign-flip[]
           :intersection[]
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
           :/[]
           :sqrt[]))
