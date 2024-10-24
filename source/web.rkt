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
         "ilinks.rkt"
         "smathml_everything.rkt"
         "smathml_comments.rkt"
         "hoffman.rkt"
         "linear_algebra_contents.rkt"
         "intro.rkt"
         "sos.rkt"
         "prot.rkt"
         "fdg.rkt"
         "sewpr.rkt"
         "dsaa.rkt"
         "timp.rkt"
         "induc.rkt"
         "ds.rkt"
         "tg.rkt"
         "sdf.rkt"
         "bps.rkt"
         "synclo.rkt"
         "topos.rkt"
         "mop.rkt"
         "series.rkt"
         "abel.rkt"
         "calculus.rkt"
         "sml-history.rkt"
         "macro.rkt"
         "huyan.rkt"
         "french.rkt"
         "maxima.rkt"
         "scheme_cg.rkt"
         "analysis1_amann.rkt"
         "cad.rkt"
         "synsem.rkt"
         "cmu15122.rkt"
         "corewar.rkt"
         "lapb.rkt"
         "order.rkt"
         "lattices.rkt"
         "lambda.rkt"
         "calculus_dieudonne.rkt"
         "oplss.rkt"
         "jones.rkt"
         "pe_jones.rkt"
         "fp_domain.rkt"
         "spt.rkt"
         "analysis_dieudonne.rkt"
         "illustrations.rkt"
         "analysis3_amann.rkt"
         "classical_complex_analysis.rkt"
         "analysis_notes.rkt"
         "fdvs.rkt"
         "linear_algebra_notes.rkt"
         "archaeology.rkt"
         "analysis_munkres.rkt"
         "cat_awodey.rkt"
         "topology.rkt"
         "combinatorics_notes.rkt"
         "asd.rkt"
         "stone_spaces.rkt"
         "denotational_semantics_notes.rkt"
         "cat_homework.rkt"
         "topology_via_logic.rkt"
         "hott.rkt"
         "useless.rkt"
         "exercises_in_analysis.rkt"
         
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
  (emitXml ilinks.html "../ilinks.html")
  (emitXml smathml_everything.html "../smathml_everything.html")
  (emitXml smathml_comments.html "../smathml_comments.html")
  (emitXml hoffman.html "../hoffman.html")
  (emitXml linear_algebra_contents.html "../linear_algebra_contents.html")
  (emitXml hoffman_ch1.html "../hoffman_ch1.html")
  (emitXml hoffman_ch2.html "../hoffman_ch2.html")
  (emitXml hoffman_ch3.html "../hoffman_ch3.html")
  (emitXml hoffman_ch4.html "../hoffman_ch4.html")
  (emitXml hoffman_ch5.html "../hoffman_ch5.html")
  (emitXml hoffman_ch6.html "../hoffman_ch6.html")
  (emitXml hoffman_ch7.html "../hoffman_ch7.html")
  (emitXml hoffman_ch8.html "../hoffman_ch8.html")
  (emitXml hoffman_ch9.html "../hoffman_ch9.html")
  (emitXml hoffman_ch10.html "../hoffman_ch10.html")
  (emitXml intro.html "../intro.html")
  (emitXml sos.html "../sos.html")
  (emitXml prot.html "../prot.html")
  (emitXml fdg.html "../fdg.html")
  (emitXml sewpr.html "../sewpr.html")
  (emitXml dsaa.html "../dsaa.html")
  (emitXml timp.html "../timp.html")
  (emitXml induc.html "../induc.html")
  (emitXml ds.html "../ds.html")
  (emitXml tg.html "../tg.html")
  (emitXml sdf.html "../sdf.html")
  (emitXml bps.html "../bps.html")
  (emitXml synclo.html "../synclo.html")
  (emitXml topos.html "../topos.html")
  (emitXml mop.html "../mop.html")
  (emitXml series.html "../series.html")
  (emitXml abel.html "../abel.html")
  (emitXml calculus.html "../calculus.html")
  (emitXml sml-history.html "../sml-history.html")
  (emitXml macro.html "../macro.html")
  (emitXml huyan.html "../huyan.html")
  (emitXml french.html "../french.html")
  (emitXml maxima.html "../maxima.html")
  (emitXml scheme_cg.html "../scheme_cg.html")
  (emitXml analysis1_amann.html "../analysis1_amann.html")
  (emitXml cad.html "../cad.html")
  (emitXml synsem.html "../synsem.html")
  (emitXml cmu15122.html "../cmu15122.html")
  (emitXml corewar.html "../corewar.html")
  (emitXml lapb.html "../lapb.html")
  (emitXml order.html "../order.html")
  (emitXml lattices.html "../lattices.html")
  (emitXml lambda.html "../lambda.html")
  (emitXml calculus_dieudonne.html "../calculus_dieudonne.html")
  (emitXml oplss.html "../oplss.html")
  (emitXml jones.html "../jones.html")
  (emitXml pe_jones.html "../pe_jones.html")
  (emitXml fp_domain.html "../fp_domain.html")
  (emitXml spt.html "../spt.html")
  (emitXml analysis_dieudonne.html "../analysis_dieudonne.html")
  (emitXml illustrations.html "../illustrations.html")
  (emitXml analysis3_amann.html "../analysis3_amann.html")
  (emitXml classical_complex_analysis.html "../classical_complex_analysis.html")
  (emitXml hoffman_appendix.html "../hoffman_appendix.html")
  (emitXml analysis_notes.html "../analysis_notes.html")
  (emitXml fdvs.html "../fdvs.html")
  (emitXml linear_algebra_notes.html "../linear_algebra_notes.html")
  (emitXml archaeology.html "../archaeology.html")
  (emitXml analysis_munkres.html "../analysis_munkres.html")
  (emitXml cat_awodey.html "../cat_awodey.html")
  (emitXml topology.html "../topology.html")
  (emitXml hoffman_preface.html "../hoffman_preface.html")
  (emitXml combinatorics_notes.html "../combinatorics_notes.html")
  (emitXml asd.html "../asd.html")
  (emitXml stone_spaces.html "../stone_spaces.html")
  (emitXml denotational_semantics_notes.html
           "../denotational_semantics_notes.html")
  (emitXml cat_homework.html "../cat_homework.html")
  (emitXml topology_via_logic.html "../topology_via_logic.html")
  (emitXml hott.html "../hott.html")
  (emitXml useless.html "../useless.html")
  (emitXml exercises_in_analysis.html "../exercises_in_analysis.html")
  
  )
(emit-web)