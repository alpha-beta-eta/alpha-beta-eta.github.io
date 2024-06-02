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
         "cat_awodey.rkt"
         "combinatorics_notes.rkt"
         "denotational_semantics_notes.rkt"
         "hott.rkt"
         "useless.rkt"
         "exercises_in_analysis.rkt"
         "blind.rkt"
         "lattice_theory_notes.rkt"
         "lattice_notes.rkt"
         "hocl.rkt"
         "macro_notes.rkt"
         "macro_dsl.rkt"
         "algebra.rkt"
         "p423.rkt"
         "clos.rkt"
         "little_typer.rkt"
         "game_design.rkt"
         "cmu15210.rkt"
         "proof_theory.rkt"
         "kerodon.rkt"
         "cltt.rkt"
         "logic.rkt"
         "metaocaml.rkt"
         "zork.rkt"
         "monad.rkt"
         "polynomial_computation.rkt"
         "measure.rkt"
         "elements_knopp.rkt"
         "classical_ai.rkt"
         "stone.rkt"
         "pointless.rkt"
         "refal.rkt"
         "zipper.rkt"
         "aimtr.rkt"
         "simple-ad.rkt"
         "linear_logic_notes.rkt"
         "qualitative_representations.rkt"
         "boolean.rkt"
         "game_semantics.rkt"
         "pihkal.rkt"
         "continuation.rkt"
         "cmu15458.rkt"
         "picture.rkt"
         "catlog.rkt"
         "game_programming.rkt"
         "implicit.rkt"
         "manifolds.rkt"
         "set_theory.rkt"
         "godel.rkt"
         "fear-of-macros.rkt"
         "reviews.rkt"
         "exercises_in_lattice_theory.rkt"
         "tspl4.rkt"
         "commalg.rkt"
         "lazy_eager_greedy.rkt"
         "nlab.rkt"
         "plan.rkt"
         "csug.rkt"
         "3imp.rkt"
         "plane_algebraic_curves.rkt"
         "game_programming_notes.rkt"
         "prafm.rkt"
         "engine.rkt"
         "ski.rkt"
         "cmu15150.rkt"
         "extensible_spec.rkt"
         "language_cognitive.rkt"
         "papers_plan.rkt"
         "having_an_effect.rkt"
         "haskell_music.rkt"
         "array.rkt"
         "algebraic_topology.rkt"
         "analysis1_godement.rkt"
         "machine_logic.rkt"
         "control.rkt"
         "automated.rkt"
         "rewrite.rkt"
         "pfpl.rkt"
         "manifolds_and_geometry.rkt"
         "msp.rkt"
         "vector_analysis.rkt"
         "abstract.rkt"
         "domain.rkt"
         "unified.rkt"
         "topology_via_logic.rkt"
         "practical_implementation.rkt"
         "object.rkt"
         "spec.rkt"
         "substructural.rkt"
         "proof-tree.rkt"
         "algebraic_subtyping.rkt"
         "constructive_logic_notes.rkt"
         "tapl.rkt"
         "aczel1997.rkt"
         "universal_coalgebra.rkt"
         "cmu.rkt"
         )
(define (emit-web)
  (parameterize ([replace? #t])
    (emitXml cmu.html "../cmu.html")
    (emitXml macro.html "../macro.html")
    (emitXml universal_coalgebra.html
             "../universal_coalgebra.html")
    (emitXml aczel1997.html
             "../aczel1997.html")
    (emitXml tapl.html "../tapl.html")
    (emitXml constructive_logic_notes.html
             "../constructive_logic_notes.html")
    (emitXml cat_awodey.html "../cat_awodey.html")
    (emitXml algebraic_subtyping.html
             "../algebraic_subtyping.html")
    (emitXml induc.html "../induc.html")
    (emitXml smathml_everything.html
             "../smathml_everything.html")
    (emitXml proof_tree_test.html
             "../proof_tree_test.html")
    (emitXml spt.html "../spt.html")
    (emitXml substructural.html
             "../substructural.html")
    (emitXml spec.html "../spec.html")
    (emitXml object.html "../object.html")
    (emitXml practical_implementation.html
             "../practical_implementation.html")
    (emitXml haskell_music.html "../haskell_music.html")
    (emitXml mop.html "../mop.html")
    (emitXml stone.html "../stone.html")
    (emitXml topology_via_logic.html
             "../topology_via_logic.html")
    (emitXml denotational_semantics_notes.html
             "../denotational_semantics_notes.html")
    (emitXml unified.html "../unified.html")
    (emitXml measure.html "../measure.html")
    (emitXml ds.html "../ds.html")
    (emitXml domain.html "../domain.html")
    (emitXml analysis1_amann.html
             "../analysis1_amann.html")
    (emitXml abstract.html "../abstract.html")
    (emitXml abel.html "../abel.html")
    (emitXml vector_analysis.html
             "../vector_analysis.html")
    (emitXml msp.html "../msp.html")
    (emitXml manifolds_and_geometry.html
             "../manifolds_and_geometry.html")
    (emitXml boolean.html "../boolean.html")
    (emitXml sicm.html "../sicm.html")
    (emitXml analysis1_godement.html
             "../analysis1_godement.html")
    (emitXml bps.html "../bps.html")
    (emitXml pfpl.html "../pfpl.html")
    (emitXml rewrite.html "../rewrite.html")
    (emitXml automated.html "../automated.html")
    (emitXml control.html "../control.html")
    (emitXml logic.html "../logic.html")
    (emitXml plan.html "../plan.html")
    (emitXml huyan.html "../huyan.html")
    (emitXml index.html "../index.html")
    (emitXml useless.html "../useless.html")
    (emitCss styles.css "../styles.css")
    (emitXml rss.xml "../rss.xml"))
  (parameterize ([replace? #f])
    (emitXml picture.html "../picture.html"))
  (emitXml some_programs.html "../some_programs.html")
  (emitXml brzozowski.html "../brzozowski.html")
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
  (emitXml tg.html "../tg.html")
  (emitXml sdf.html "../sdf.html")
  (emitXml synclo.html "../synclo.html")
  (emitXml topos.html "../topos.html")
  (emitXml series.html "../series.html")
  (emitXml calculus.html "../calculus.html")
  (emitXml sml-history.html "../sml-history.html")
  (emitXml french.html "../french.html")
  (emitXml maxima.html "../maxima.html")
  (emitXml scheme_cg.html "../scheme_cg.html")
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
  (emitXml analysis_dieudonne.html "../analysis_dieudonne.html")
  (emitXml illustrations.html "../illustrations.html")
  (emitXml analysis3_amann.html "../analysis3_amann.html")
  (emitXml classical_complex_analysis.html "../classical_complex_analysis.html")
  (emitXml hoffman_appendix.html "../hoffman_appendix.html")
  (emitXml analysis_notes.html "../analysis_notes.html")
  (emitXml fdvs.html "../fdvs.html")
  (emitXml linear_algebra_notes.html "../linear_algebra_notes.html")
  (emitXml hoffman_preface.html "../hoffman_preface.html")
  (emitXml combinatorics_notes.html "../combinatorics_notes.html")
  (emitXml hott.html "../hott.html")
  (emitXml exercises_in_analysis.html "../exercises_in_analysis.html")
  (emitXml blind.html "../blind.html")
  (emitCss french.css "../french.css")
  (emitXml lattice_theory_notes.html "../lattice_theory_notes.html")
  (emitXml lattice_notes.html "../lattice_notes.html")
  (emitXml hocl.html "../hocl.html")
  (emitXml macro_notes.html "../macro_notes.html")
  (emitXml macro_dsl.html "../macro_dsl.html")
  (emitXml algebra.html "../algebra.html")
  (emitXml p423.html "../p423.html")
  (emitXml clos.html "../clos.html")
  (emitXml little_typer.html "../little_typer.html")
  (emitXml game_design.html "../game_design.html")
  (emitXml cmu15210.html "../cmu15210.html")
  (emitXml proof_theory.html "../proof_theory.html")
  (emitXml kerodon.html "../kerodon.html")
  (emitXml cltt.html "../cltt.html")
  (emitXml metaocaml.html "../metaocaml.html")
  (emitXml zork.html "../zork.html")
  (emitXml monad.html "../monad.html")
  (emitXml polynomial_computation.html "../polynomial_computation.html")
  (emitXml elements_knopp.html "../elements_knopp.html")
  (emitXml classical_ai.html "../classical_ai.html")
  (emitXml pointless.html "../pointless.html")
  (emitXml refal.html "../refal.html")
  (emitXml zipper.html "../zipper.html")
  (emitXml aimtr.html "../aimtr.html")
  (emitXml simple-ad.html "../simple-ad.html")
  (emitXml linear_logic_notes.html "../linear_logic_notes.html")
  (emitXml qualitative_representations.html "../qualitative_representations.html")
  (emitXml game_semantics.html "../game_semantics.html")
  (emitXml pihkal.html "../pihkal.html")
  (emitXml continuation.html "../continuation.html")
  (emitXml cmu15458.html "../cmu15458.html")
  (emitXml catlog.html "../catlog.html")
  (emitXml game_programming.html "../game_programming.html")
  (emitXml implicit.html "../implicit.html")
  (emitXml manifolds.html "../manifolds.html")
  (emitXml set_theory.html "../set_theory.html")
  (emitXml godel.html "../godel.html")
  (emitXml fear-of-macros.html "../fear-of-macros.html")
  (emitXml reviews.html "../reviews.html")
  (emitXml exercises_in_lattice_theory.html
           "../exercises_in_lattice_theory.html")
  (emitXml tspl4.html "../tspl4.html")
  (emitXml commalg.html "../commalg.html")
  (emitXml lazy_eager_greedy.html "../lazy_eager_greedy.html")
  (emitXml nlab.html "../nlab.html")
  (emitXml csug.html "../csug.html")
  (emitXml 3imp.html "../3imp.html")
  (emitXml plane_algebraic_curves.html
           "../plane_algebraic_curves.html")
  (emitXml game_programming_notes.html
           "../game_programming_notes.html")
  (emitXml prafm.html "../prafm.html")
  (emitXml engine.html "../engine.html")
  (emitXml ski.html "../ski.html")
  (emitXml cmu15150.html "../cmu15150.html")
  (emitXml extensible_spec.html "../extensible_spec.html")
  (emitXml language_cognitive.html "../language_cognitive.html")
  (emitXml papers_plan.html "../papers_plan.html")
  (emitXml having_an_effect.html "../having_an_effect.html")
  (emitXml array.html "../array.html")
  (emitXml algebraic_topology.html
           "../algebraic_topology.html")
  (emitXml machine_logic.html
           "../machine_logic.html")
  )
(emit-web)
#;
(parameterize ([replace? #t])
  (emit-web))