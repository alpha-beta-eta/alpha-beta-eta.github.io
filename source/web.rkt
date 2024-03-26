#lang racket
(require SMathML
         "styles.rkt"
         "index.rkt"
         "linear_algebra.rkt"
         "brzozowski.rkt"
         "programming.rkt"
         "some_programs.rkt"
         "sicm.rkt"
         "linear_algebra_exercises.rkt"
         "eopl.rkt"
         "smathml.rkt"
         "geometric_algebra.rkt"
         "lem_editor.rkt"
         "zil.rkt"
         "curry-howard.rkt"
         "Ship-of-Theseus.rkt"
         "misc.rkt"
         "computation_structures.rkt"
         "books.rkt"
         "rss.xml.rkt"
         "links.rkt"
         "smathml_ref.rkt"
         
         )
(define (emit-web)
  (emitCss styles.css "../styles.css")
  (emitXml rss.xml "../rss.xml")
  (emitXml index.html "../index.html")
  (emitXml some_programs.html "../some_programs.html")
  (emitXml brzozowski.html "../brzozowski.html")
  (emitXml sicm.html "../sicm.html")
  (emitXml programming.html "../programming.html")
  (emitXml linear_algebra.html "../linear_algebra.html")
  (emitXml linear_algebra_exercises.html "../linear_algebra_exercises.html")
  (emitXml eopl.html "../eopl.html")
  (emitXml smathml.html "../smathml.html")
  (emitXml geometric_algebra.html "../geometric_algebra.html")
  (emitXml lem_editor.html "../lem_editor.html")
  (emitXml zil.html "../zil.html")
  (emitXml curry-howard.html "../curry-howard.html")
  (emitXml Ship-of-Theseus.html "../Ship-of-Theseus.html")
  (emitXml misc.html "../misc.html")
  (emitXml computation_structures.html "../computation_structures.html")
  (emitXml books.html "../books.html")
  (emitXml links.html "../links.html")
  (emitXml smathml_ref.html "../smathml_ref.html")
  
  )
(emit-web)