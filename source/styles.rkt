#lang racket
(provide styles.css)
(define styles.css
  '(("@font-face"
     (font-family "LatinModernMath")
     (src "url('fonts/LatinModernMath.woff2') format('woff2')"))
    ("@font-face"
     (font-family "Consolas")
     (src "url('fonts/Consolas.woff2') format('woff2')"))
    ("@font-face"
     (font-family "SourceHanSansSC-VF")
     (src "url('fonts/SourceHanSansSC-VF.otf.woff2') format('woff2')"))
    ("body"
     (font-family "SourceHanSansSC-VF")
     (font-weight "350"))
    ("h1, h2, h3, h4, h5, h6, a, b"
     (font-weight "475"))
    ("code"
     (font-family "Consolas")
     (color "white")
     (background-color "black"))
    ("pre:has(> code)"
     (background-color "black"))
    (".qed"
     (text-align "right"))
    ("math"
     (font-family "LatinModernMath"))
    ("mtext"
     (font-family "SourceHanSansSC-VF")
     (font-weight "300"))
    ("cite"
     (font-style "normal"))
    ))