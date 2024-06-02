#lang racket
(provide styles.css)
(define styles.css
  '(("@font-face"
     (font-family "LatinModern")
     (src "url('fonts/LatinModernMath.woff2') format('woff2')"))
    ("@font-face"
     (font-family "JBMono")
     (src "url('fonts/JetBrainsMono-Light.woff2') format('woff2')"))
    ("@font-face"
     (font-family "SHSerif")
     (src "url('fonts/SourceHanSerifCN-VF.otf.woff2') format('woff2')"))
    ("body"
     (font-family "SHSerif")
     (font-weight "400")
     (quotes "'\\\"' '\\\"'"))
    ("h1, h2, h3, h4, h5, h6, a, b"
     (font-weight "700"))
    ("a"
     (text-decoration "none"))
    ("code"
     (font-family "JBMono")
     (color "white")
     (background-color "black")
     (font-variant-ligatures "none"))
    ("pre:has(> code)"
     (background-color "black"))
    ("em"
     (font-style "normal")
     (font-family "Courier, KaiTi"))
    ("math"
     (font-family "LatinModern, SHSerif"))
    ("mtext"
     (font-weight "400"))
    ("ms"
     (font-family "JBMono")
     (color "white")
     (background-color "black"))
    (".qed"
     (text-align "right"))
    ("cite"
     (font-style "normal"))
    (".slide"
     (break-inside "avoid")
     (border "2px solid black")
     (width "400px")
     (height "225px"))
    (".label"
     (break-inside "avoid"))
    (".dialogue"
     (width "100%")
     (border-top "1.5px solid black")
     (break-inside "avoid"))
    (".law"
     (width "100%")
     (border-top "1.5px solid black")
     (break-inside "avoid"))
    ("table.law blockquote"
     (width "60%")
     (margin "auto")
     (padding "10px")
     (border "5px double black"))
    (".leftd"
     (width "48%"))
    (".middled"
     (width "15px"))
    (".dialogue td"
     (padding "5px")
     (text-align "left")
     (vertical-align "top"))
    (".dashed"
     (border "2.5px dashed white"))
    ))