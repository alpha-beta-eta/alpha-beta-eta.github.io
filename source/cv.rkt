#lang racket
(provide cv.html)
(require SMathML)
(define (Tdl . html*)
  `(td ((align "left")) . ,html*))
(define (Tdr . html*)
  `(td ((align "right")) . ,html*))
(define cv.html
  (TmPrelude
   #:title "cv"
   #:css "styles.css"
   (H1 "Shuyan Bian")
   (Table
    #:attr* '((width "100%"))
    (Tr (Tdl "Email: bianshuyan42@gmail.com")
        (Tdr "GitHub: "
             (A "alpha-beta-eta"
                #:attr* '((href "https://github.com/alpha-beta-eta"))))))
   (H2 "Education")
   (Table
    #:attr* '((width "100%"))
    (Tr (Tdl (B "Northeastern University (Boston)"))
        (Tdr "2024-2026"))
    (Tr (Tdl "M.S. in Software Engineering"))
    (Tr (Tdl (B "Nanjing University of Information Science &amp; Technology"))
        (Tdr "2018-2023"))
    (Tr (Tdl "B.S. in Information and Computing Science")))
   (H2 "Interests")
   (Ul (Li "Programming Pedagogy")
       (Li "Compiler Construction")
       (Li "Domain-Specific Languages")
       (Li "Proof Assistants"))
   (H2 "Projects")
   (Ul (Li "SMathML: "
           (A "alpha-beta-eta/SMathML"
              #:attr* '((href "https://github.com/alpha-beta-eta/SMathML"))))
       )
   ))