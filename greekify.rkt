#lang racket

;(require racket/match)
;(require racket/cmdline)

(define nl (list->string (list #\newline)))
(define help
  (string-append nl
"Greekify" nl
"by Matthew Dirks" nl
"Version 0.3 (March 2013)" nl
nl
"There is a paucity of options for typing in polytonic Greek in Linux. "
"Although there is a polytonic Greek keyboard layout, it lacks "
"breathing marks and the ability to put multiple diacritic marks on "
"a single character (e.g. a circumflex and an iota subscript). "
"This program takes ASCII input and "
"transliterates it into Greek, loosely following the conventions on the "
"Tufts Perseus Greek Word Study Tool website "
"(http://www.perseus.tufts.edu/hopper/morph, accessed 1/9/2013). "
"Specifically, we map " nl
" alpha   : a" nl
" beta    : b" nl
" gamma   : g" nl
" delta   : d" nl
" epsilon : e" nl
" zeta    : z" nl
" eta     : h" nl
" theta   : q" nl
" iota    : i" nl
" kappa   : k" nl
" lambda  : l" nl
" mu      : m" nl
" nu      : n" nl
" xi      : c" nl
" omicron : o" nl
" pi      : p" nl
" rho     : r" nl
" medial sigma (σ) : s" nl
" terminal sigma (ς) : w" nl
" tau      : t" nl
" upsilon  : u" nl
" phi      : f" nl
" chi      : x" nl
" psi      : y" nl
" omega    : v" nl
" majuscule digamma  : #" nl
" minuscule digamma  : 3" nl
" majuscule koppa : %" nl
" minuscule koppa : 5" nl
" interpunct (·) : *"nl
nl
"Vowels and rho can be modified with breathings, accents, and iota "
"subscripts by putting special modifier characters before them:" nl
" acute            : /" nl
" grave            : -" nl
" circumflex       : =" nl
" smooth breathing : (" nl
" rough breathing  : )" nl
" iota subscript   : |" nl
nl
"Note that the order of modifications does not matter." nl
"Modifiers cannot be used on characters that have already " nl
"been transliterated. Hence, " nl
"    greekify \"μ=ηνιν\"" nl
"yields"nl
"    \"μ=ηνιν\"." nl
nl
"Important deviations from Perseus: " nl
"*While Perseus requires modifiers to succeed the letter "
"they modify, we expect modifiers to precede the letter they modify." nl
"*While Perseus does not distinguish between "
"ς and σ, we transliterate w->ς and s->σ. " nl
"*Perseus transliterates w->ω; since this conflicts "
"with our convention w->ς, we substitue v->ω." nl
"*Because of conflicts with the escape character, the modifier "
"for our graves is -, not \\." nl
"De gustibus non est disputandum. " nl
nl
"The text to transliterate is passed as a command line argument "
"contained in quotation marks. "
"For example, running" nl
"   greekify \"m=hnin )/aeide qe-a Phlhi/adev )Axil=how\"" nl
"yields" nl
"   μῆνιν ἄειδε θεὰ Πηληιάδεω Ἀχιλῆος" nl
nl
"To report bugs or other errors, please contact Matthew Dirks at "
"majadirks@gmail.com" nl nl))

;;is-among?: char (listof char) -> bool
;;Checks whether a character is in a given list of characters
(define (is-among? c cs)
  (and
   (not (empty? (member c cs)))
   (not (false? (member c cs)))))

;;modifiers: (listof char)
;;listt of acceptable modifiers
(define modifiers 
  '(#\/ #\- #\= #\( #\) #\|))

;;can-take-acute: (listof char)
(define can-take-acute
         '(#\A #\a
               #\E #\e
               #\H #\h
               #\I #\i
               #\O #\o
               #\U #\u
               #\V #\v))
;;can-take-grave: (listof char)
(define can-take-grave can-take-acute)
;;can-take-circ: (listof char)
(define can-take-circ can-take-acute)
;;can-take-smooth: (listof char)
(define can-take-smooth can-take-acute)
;;can-take-rough: (listof char)
(define can-take-rough (cons #\R (cons #\r can-take-acute)))
;;can-take-iota: (listof char)
(define can-take-iota '(#\A #\a #\H #\h #\V #\v))  
  
;;transliterate-letter-to-char: string -> char
;;Given a string of English characters,
;;transliterates the first letter to the corresponding
;;Greek letter. Special characters can precede
;;a vowel to affix diacritic marks, as follows:
;; /          : acute
;; -          : grave
;; =          : circumfles
;; )          : smooth breathing
;; (          : rough breathing
;; |          : iota subscrips
;;
;; Rough breathings can also precede and modify
;; the letter ρ (rho).
(define (transliterate-letter-to-char str)
  (local
   {
    ;;list-of-char: listof char (makes sense, huh?)
    ;;list of characters in the given string.
    (define list-of-char (string->list str))
    
    ;;transliterate-aux: 
    ;;       (listof char) bool bool bool bool bool bool -> char
    ;;If the first character of the string is a consonant besides rho,
    ;;return the Greek transliteration.
    ;;If it is rho, return ρ if rough? is false,
    ;;and return ῥ if rough? is true.
    ;;Similarly, for vowels, check the parameters to determine which
    ;;diacritics to append.
    ;;If transliterate-aux receives a modifier, recursively call itself
    ;;with the relevant boolean set to true. 
    (define (transliterate-aux chars ac? gr? circ? smooth? rough? iota?)
      (local
        {(define null-accent? (and (not ac?) (not gr?) (not circ?)))
         (define null-breathing? (and (not smooth?) (not rough?)))
         (define null-iota? (not iota?))
         (define head (first chars))
         (define modifying? (> (length chars) 1))
         ;;head-is?:char -> bool
         ;;checks if head matches given char
         (define (head-is? c)
           (char=? head c))
         ;;head-is-modifier?: char -> bool
         ;;checks if head is a modifier matching the given char
         (define (head-is-modifier? c)
           (and
            (head-is? c)
            modifying?))
         }
        (cond
          ;; modifiers
          ;;acute
          [(head-is-modifier? #\/)
           (transliterate-aux
            (rest chars) true false false smooth? rough? iota?)]
          ;;grave
          [(head-is-modifier? #\-)
           (transliterate-aux
            (rest chars) false true false smooth? rough? iota?)]
          ;;circumflex
          [(head-is-modifier? #\=)
           (transliterate-aux
            (rest chars) false false true smooth? rough? iota?)]
          ;;smooth
          [(head-is-modifier? #\))
           (transliterate-aux
            (rest chars) ac? gr? circ? true false iota?)]
          ;;rough
          [(head-is-modifier? #\()
           (transliterate-aux
            (rest chars) ac? gr? circ? false true iota?)]
          ;;iota subscript
          [(head-is-modifier? #\|)
           (transliterate-aux
            (rest chars) ac? gr? circ? smooth? rough? true)]
          [else
           (match head
             ;;non-rhota consonants
             [#\B #\B] [#\b #\β]
             [#\G #\Γ] [#\g #\γ]
             [#\D #\Δ] [#\d #\δ]
             [#\Z #\Z] [#\z #\ζ]
             [#\Q #\Θ] [#\q #\θ]
             [#\K #\Κ] [#\k #\κ]
             [#\L #\Λ] [#\l #\λ]
             [#\M #\Μ] [#\m #\μ]
             [#\N #\Ν] [#\n #\ν]
             [#\C #\Ξ] [#\c #\ξ]
             [#\P #\Π] [#\p #\π]
             [#\S #\Σ] [#\s #\σ] [#\w #\ς]
             [#\T #\Τ] [#\t #\τ]
             [#\F #\Φ] [#\f #\φ]
             [#\X #\Χ] [#\x #\χ]
             [#\Y #\Ψ] [#\y #\ψ]
             [#\# #\Ϝ] [#\3 #\ϝ]
             [#\% #\Ϙ] [#\5 #\ϙ]
             [#\* #\·]
             ;;rhota
             [#\R
              (cond
                [rough? #\Ῥ]
                [else #\Ρ])]
             [#\r
              (cond
                [rough? #\ῥ]
                [smooth? #\ῤ]
                [else #\ρ])]
             
             ;;vowels
             [#\A
              (cond
                [(and smooth? iota? ac?) #\ᾌ]
                [(and smooth? iota? gr?) #\ᾊ]
                [(and smooth? iota? circ?) #\ᾎ]
                [(and smooth? iota? null-accent?) #\ᾈ]
                [(and smooth? null-iota? ac?) #\Ἄ]
                [(and smooth? null-iota? gr?) #\Ἂ]
                [(and smooth? null-iota? circ?) #\Ἆ]
                [(and smooth? null-iota? null-accent?) #\Ἀ]
                [(and rough? iota? ac?) #\ᾉ]
                [(and rough? iota? gr?) #\ᾋ]
                [(and rough? iota? circ?) #\ᾏ]
                [(and rough? iota? null-accent?) #\ᾉ]
                [(and rough? null-iota? ac?) #\Ἅ]
                [(and rough? null-iota? gr?) #\Ἃ]
                [(and rough? null-iota? circ?) #\Ἇ]
                [(and rough? null-iota? null-accent?) #\Ἁ]
                [(and null-breathing? iota? (not null-accent?))
                 (error 
                  (string-append "Accented A with iota, null-breathing: "
                                 "unimplemented."))]
                [(and null-breathing? iota? null-accent?) #\ᾼ]
                [(and null-breathing? null-iota? ac?) #\Ά]
                [(and null-breathing? null-iota? gr?) #\Ὰ]
                [(and null-breathing? null-iota? circ?)
                 ;;TODO Implement if possible
                 (error (string-append ;"No Unicode character for "
                         "Circumflexed majuscule alpha "
                         "without breathing "
                         "unimplemented."))]
                [(and null-breathing? null-iota? null-accent?) #\A])]
             
             [#\a
              (cond
                [(and smooth? iota? ac?) #\ᾄ]
                [(and smooth? iota? gr?) #\ᾂ]
                [(and smooth? iota? circ?) #\ᾆ]
                [(and smooth? iota? null-accent?) #\ᾀ]
                [(and smooth? null-iota? ac?) #\ἄ]
                [(and smooth? null-iota? gr?) #\ἂ]
                [(and smooth? null-iota? circ?) #\ἆ]
                [(and smooth? null-iota? null-accent?) #\ἀ]
                [(and rough? iota? ac?) #\ᾅ]
                [(and rough? iota? gr?) #\ᾃ]
                [(and rough? iota? circ?) #\ᾇ]
                [(and rough? iota? null-accent?) #\ᾁ]
                [(and rough? null-iota? ac?) #\ἅ]
                [(and rough? null-iota? gr?) #\ἃ]
                [(and rough? null-iota? circ?) #\ἇ]
                [(and rough? null-iota? null-accent?) #\ἁ]
                [(and null-breathing? iota? ac?) #\ᾴ]
           [(and null-breathing? iota? gr?) #\ᾲ]
           [(and null-breathing? iota? circ?) #\ᾷ]
           [(and null-breathing? iota? null-accent?) #\ᾳ]
           [(and null-breathing? null-iota? ac?) #\ά]
           [(and null-breathing? null-iota? gr?) #\ὰ]
           [(and null-breathing? null-iota? circ?) #\ᾶ]
           [(and null-breathing? null-iota? null-accent?) #\α])]
             
             [#\E
              (cond
                [iota? (error "Majuscule epsilons cannot have iota subscripts.")]
                [circ? (error "Majuscule epsilons cannot have circumflexes.")]
                [(and smooth? ac?) #\Ἔ]
                [(and smooth? gr?) #\Ἒ]
                [(and smooth? null-accent? ) #\Ἐ]
                [(and rough? ac?) #\Ἕ]
                [(and rough? gr?) #\Ἓ]
                [(and rough? null-accent?) #\Ἑ]
                [(and null-breathing? ac?) #\Έ]
                [(and null-breathing? gr?) #\Ὲ]
                [(and null-breathing? null-accent?) #\Ε])]
             [#\e
              (cond
                [circ? (error "Epsilons cannot have circumflexes!")]
                [iota? (error "Epsilons cannot have iota subscripts!")]      
                [(and smooth? ac?) #\ἔ]
                [(and smooth? gr?) #\ἒ]
                [(and smooth? null-accent?) #\ἐ]
                [(and rough? ac?) #\ἕ]
                [(and rough? gr?) #\ἒ]
                [(and rough? null-accent?) #\ἑ]
                [(and null-breathing? ac?) #\έ]
           [(and null-breathing? gr?) #\ὲ]
           [(and null-breathing? null-accent?) #\ε])]
             
             [#\H
              (cond
                [(and smooth? iota? ac?) #\ᾜ]
                [(and smooth? iota? gr?) #\ᾚ]
                [(and smooth? iota? circ?) #\ᾞ]
                [(and smooth? iota? null-accent?) #\ᾘ]
                [(and smooth? null-iota? ac?) #\Ἤ]
                [(and smooth? null-iota? gr?) #\Ἢ]
                [(and smooth? null-iota? circ?) #\Ἦ]
                [(and smooth? null-iota? null-accent?) #\Ἠ]
                [(and rough? iota? ac?) #\ᾝ]
                [(and rough? iota? gr?) #\ᾛ]
                [(and rough? iota? circ?) #\ᾟ]
                [(and rough? iota? null-accent?) #\ᾙ]
                [(and rough? null-iota? ac?) #\Ἥ]
                [(and rough? null-iota? gr?) #\Ἣ]
                [(and rough? null-iota? circ?) #\Ἧ]
                [(and rough? null-iota? null-accent?) #\Ἡ]
                [(and null-breathing? iota? (not null-accent?))
                 (error 
                  (string-append "No Unicode character for accented H "
                                 "with iota subscript "
                                 "but no breathing mark."))]
                [(and null-breathing? iota? null-accent?) #\ῌ]
                [(and null-breathing? null-iota? ac?) #\Ή]
                [(and null-breathing? null-iota? gr?) #\Ὴ]
                [(and null-breathing? null-iota? circ?)
                 (error (string-append "No Unicode character for "
                                       "circumflexed H "
                                       "without breathing."))]
                [(and null-breathing? null-iota? null-accent?) #\Η])] 
             
             [#\h
              (cond
                [(and smooth? iota? ac?) #\ᾔ]
                [(and smooth? iota? gr?) #\ᾒ]
                [(and smooth? iota? circ?) #\ᾖ]
                [(and smooth? iota? null-accent?) #\ᾐ]
                [(and smooth? null-iota? ac?) #\ἤ]
                [(and smooth? null-iota? gr?) #\ἢ]
                [(and smooth? null-iota? circ?) #\ἦ]
                [(and smooth? null-iota? null-accent?) #\ἠ]
                [(and rough? iota? ac?) #\ᾕ]
                [(and rough? iota? gr?) #\ᾓ]
                [(and rough? iota? circ?) #\ᾗ]
                [(and rough? iota? null-accent?) #\ᾑ]
                [(and rough? null-iota? ac?) #\ἥ]
                [(and rough? null-iota? gr?) #\ἣ]
                [(and rough? null-iota? circ?) #\ἧ]
                [(and rough? null-iota? null-accent?) #\ἡ]
                [(and null-breathing? iota? ac?) #\ῄ]
                [(and null-breathing? iota? gr?) #\ῂ]
                [(and null-breathing? iota? circ?) #\ῇ]
                [(and null-breathing? iota? null-accent?) #\ῃ]
                [(and null-breathing? null-iota? ac?) #\ή]
                [(and null-breathing? null-iota? gr?) #\ὴ]
                [(and null-breathing? null-iota? circ?) #\ῆ]
                [(and null-breathing? null-iota? null-accent?) #\η])]
             
             [#\I
              (cond
                [iota? (error "Iotas cannot have iota subscripts!")]
                [(and smooth? ac?) #\Ἴ]
                [(and smooth? gr?) #\Ἲ]
                [(and smooth? circ?) #\Ἶ]
                [(and smooth? null-accent?) #\Ἰ]
                [(and rough? ac?) #\Ἵ]
                [(and rough? gr?) #\Ἳ]
                [(and rough? circ?) #\Ἷ]
                [(and rough? null-accent?) #\Ἱ]
                [(and null-breathing? ac?) #\Ί]
                [(and null-breathing? gr?) #\Ὶ]
                [(and null-breathing? circ?) 
                 (error (string-append "Majuscule iota with circumflex "
                                       "but no breathing: unimplemented."))]
           [(and null-breathing? null-accent?) #\Ι])]

             [#\i
              (cond
                [iota? (error "Iotas cannot have iota subscripts!")]
                [(and smooth? ac?) #\ἴ]
                [(and smooth? gr?) #\ἲ]
                [(and smooth? circ?) #\ἶ]
                [(and smooth? null-accent?) #\ἰ]
                [(and rough? ac?) #\ἵ]
           [(and rough? gr?) #\ἳ]
           [(and rough? circ?) #\ἷ]
           [(and rough? null-accent?) #\ἱ]
           [(and null-breathing? ac?) #\ί]
           [(and null-breathing? gr?) #\ὶ]
           [(and null-breathing? circ?) #\ῖ]
           [(and null-breathing? null-accent?) #\ι])]
             
             [#\O
              (cond
                [iota? (error "Omicrons cannot have iota subscripts!")]
                [circ? (error "Omicrons cannot have circumflexes!")]
                [(and smooth? ac?) #\Ὄ]
                [(and smooth? gr?) #\Ὂ]
                [(and smooth? (not ac?) (not gr?) ) #\Ὀ]
                [(and rough? ac?) #\Ὅ]
                [(and rough? gr?) #\Ὃ]
                [(and rough? (not ac?) (not gr?)) #\Ὁ]
                [(and (not smooth?) (not rough?) ac?) #\Ό]
                [(and (not smooth?) (not rough?) gr?) #\Ὸ]
                [(and (not smooth?) (not rough?)
                      (not ac?) (not gr?)) #\Ο])]
             [#\o
              (cond
                [iota? (error "Omicrons cannot have iota subscripts!")]
                [circ? (error "Omicrons cannot have circumflexes!")]
                [(and smooth? ac?) #\ὄ]
                [(and smooth? gr?) #\ὂ]
                [(and smooth? (not ac?) (not gr?)) #\ὀ]
                [(and rough? ac?) #\ὅ]
                [(and rough? gr?) #\ὃ]
                [(and rough? (not ac?) (not gr?)) #\ὁ]
                [(and (not smooth?) (not rough?) ac?) #\ό]
                [(and (not smooth?) (not rough?) gr?) #\ὸ]
                [(and (not smooth?) (not rough?)
                      (not ac?) (not gr?)) #\ο])]
             
             [#\U
              (cond
                [smooth? (error (string-append "No Unicode character for "
                                               "majuscule upsilon with "
                                               "smooth breathing."))]
                [iota? (error "Upsilon cannot have an iota subscript.")]
                [(and rough? ac?) #\Ὕ]
                [(and rough? gr?) #\Ὓ]
                [(and rough? circ?) #\Ὗ]
                [(and rough?
                      (not ac?) (not gr?) (not circ?)) #\Ὑ]
                [(and (not smooth?) (not rough?) ac?) #\Ύ]
                [(and (not smooth?) (not rough?) gr?) #\Ὺ]
                [(and (not smooth?) (not rough?) circ?)
                 (error (string-append "No Unicode character for "
                                       "circumflexed Y "
                                       "without breathing."))]
                [(and (not smooth?) (not rough?)
                      (not ac?) (not gr?) (not circ?)) #\Υ])] 
             [#\u
              (cond
                [iota? (error "Upsilon cannot have an iota subscript.")]
                [(and smooth? ac?) #\ὔ]
                [(and smooth? gr?) #\ὒ]
                [(and smooth? circ?) #\ὖ]
                [(and smooth?
                      (not ac?) (not gr?) (not circ?)) #\ὐ]
                [(and rough? ac?) #\ὕ]
                [(and rough? gr?) #\ὓ]
                [(and rough? circ?) #\ὗ]
                [(and rough?
                      (not ac?) (not gr?) (not circ?)) #\ὑ]
                [(and (not smooth?) (not rough?) ac?) #\ύ]
                [(and (not smooth?) (not rough?) gr?) #\ὺ]
                [(and (not smooth?) (not rough?) circ?) #\ῦ]
                [(and (not smooth?) (not rough?)
                      (not ac?) (not gr?) (not circ?)) #\υ])]
             
             [#\V
              (cond
                [(and smooth? iota? ac?) #\ᾬ]
                [(and smooth? iota? gr?) #\ᾪ]
                [(and smooth? iota? circ?) #\ᾮ]
                [(and smooth? iota? (not ac?) (not gr?) (not circ?)) #\ᾨ]
                [(and smooth? (not iota?) ac?) #\Ὤ]
                [(and smooth? (not iota?) gr?) #\Ὢ]
                [(and smooth? (not iota?) circ?) #\Ὦ]
                [(and smooth? (not iota?)
                      (not ac?) (not gr?) (not circ?)) #\Ὠ]
                
                [(and rough? iota? ac?) #\ᾭ]
                [(and rough? iota? gr?) #\ᾫ]
                [(and rough? iota? circ?) #\ᾯ]
                [(and rough? iota? 
                      (not ac?) (not gr?) (not circ?)) #\ᾩ]
           [(and rough? (not iota?) ac?) #\Ὥ]
           [(and rough? (not iota?) gr?) #\Ὣ]
           [(and rough? (not iota?) circ?) #\Ὧ]
           [(and rough? (not iota?)
                 (not ac?) (not gr?) (not circ?)) #\Ὡ]
           [(and (not smooth?) (not rough?) iota? (or ac? gr? circ?))
            (error 
             (string-append 
              "No Unicode character for accented "
              "majuscule omega"
              "with iota subscript "
              "but no breathing mark."))]
           [(and (not smooth?) (not rough?) iota?
                 (not ac?) (not gr?) (not circ?)) #\ῼ]
           [(and (not smooth?) (not rough?) (not iota?) ac?) #\Ώ]
           [(and (not smooth?) (not rough?) (not iota?) gr?) #\Ὼ]
           [(and (not smooth?) (not rough?) (not iota?) circ?)
            (error (string-append "No Unicode character for "
                                  "circumflexed majuscule Omega "
                                  "without breathing."))]
           [(and (not smooth?) (not rough?) (not iota?)
                 (not ac?) (not gr?) (not circ?)) #\Ω])] 
             [#\v
              (cond
                [(and smooth? iota? ac?) #\ᾤ]
                [(and smooth? iota? gr?) #\ᾢ]
                [(and smooth? iota? circ?) #\ᾦ]
                [(and smooth? iota? (not ac?) (not gr?) (not circ?)) #\ᾠ]
                [(and smooth? (not iota?) ac?) #\ὤ]
                [(and smooth? (not iota?) gr?) #\ὢ]
                [(and smooth? (not iota?) circ?) #\ὦ]
                [(and smooth? (not iota?)
                      (not ac?) (not gr?) (not circ?)) #\ὠ]
                
                [(and rough? iota? ac?) #\ᾥ]
                [(and rough? iota? gr?) #\ᾣ]
                [(and rough? iota? circ?) #\ᾧ]
                [(and rough? iota? 
                      (not ac?) (not gr?) (not circ?)) #\ᾡ]
           [(and rough? (not iota?) ac?) #\ὥ]
           [(and rough? (not iota?) gr?) #\ὣ]
           [(and rough? (not iota?) circ?) #\ὧ]
           [(and rough? (not iota?)
                 (not ac?) (not gr?) (not circ?)) #\ὡ]
           [(and (not smooth?) (not rough?) iota? ac?) #\ῴ]
           [(and (not smooth?) (not rough?) iota? gr?) #\ῲ]
           [(and (not smooth?) (not rough?) iota? circ?) #\ῷ]
           [(and (not smooth?) (not rough?) iota?
                 (not ac?) (not gr?) (not circ?)) #\ῳ]
           [(and (not smooth?) (not rough?) (not iota?) ac?) #\ώ]
           [(and (not smooth?) (not rough?) (not iota?) gr?) #\ὼ]
           [(and (not smooth?) (not rough?) (not iota?) circ?) #\ῶ]
           [(and (not smooth?) (not rough?) (not iota?)
                 (not ac?) (not gr?) (not circ?)) #\ω])]  
             
             ;;If character is unknown, just return it
             [else (first chars)])])))}
        
        (transliterate-aux list-of-char false false false false false false)))

;;transliterate-letter-to-string: string -> string
;;Given a letter, transliterate it to string format
(define (transliterate-letter-to-string str)
         (list->string (list (transliterate-letter-to-char str))))

;;first-letter-ignore-modifiers: string->char
;;Given a string of ASCII characters, return
;;the first letter, but not any modifiers.
;;e.g. (first-letter-ignore-modifiers ")/EYILON") returns "E"
;;If no character such is found, returns first character in string.
(define (first-letter-ignore-modifiers str)
  (local
    { ;;flim-aux: (listof char) -> (listof char)
     ;;Same as main function, but works on character level
     (define (flim-aux chars)
       (cond
         ;;If we are at the end of the string, return #\0
         [(empty? chars) #\0]
         ;;If we have a modifier, look further
         [(is-among? (first chars) modifiers)
          (flim-aux (rest chars))]
         ;;Otherwise, return the first character
         [else (first chars)]))
     (define mychar (flim-aux (string->list str)))}
    (cond
      [(char=? mychar #\0) (first (string->list str))]
      [else mychar])))
         
;;first-letter: string->string
;;Given a string of ASCII characters, return any initial modifiers
;;and the first letter. e.g. (first-letter ")/EYILON") returns ")/E"
(define (first-letter str)
  (local  
    {
     (define letter (first-letter-ignore-modifiers str)) 
     ;;first-letter-aux: (listof char) -> (listof char)
     ;;Same as main function, but works on character level
     (define (first-letter-aux chars)
       (local
         {(define head (first chars))
          ;;proceed-with: (listof char) -> (listof char)
          ;;Given a list of characters, returns the first letter
          ;;and any modifiers, starting with "head"
          (define (proceed-with chs)
            (cons head (first-letter-aux (rest chs))))}
         (cond
           [(or
             (and 
              (char=? head #\/)
              (is-among? letter can-take-acute))
             (and
              (char=? head #\-)
              (is-among? letter can-take-grave))
             (and
              (char=? head #\=)
              (is-among? letter can-take-circ))
             (and
              (char=? head #\()
              (is-among? letter can-take-rough))
             (and
              (char=? head #\))
              (is-among? letter can-take-smooth))
             (and
              (char=? head #\|)
              (is-among? letter can-take-iota)))
            (proceed-with chars)]
           [else
            (list head)])))}
         ;  [(is-among? head modifiers) ;;(or #\/ #\- #\= #\( #\) #\|)
         ;   (cons head (first-letter-aux (rest chars)))]
         ;  [else (list head)])))}
    
    (list->string (first-letter-aux (string->list str)))))

;;rest-letter: string->string
;;Given a string, truncates the first letter and returns the rest.
(define (rest-letter str)
  (local
    {(define secandum (string->list (first-letter str)))
     (define my-list (string->list str))
     ;;rest-letter-aux: (listof char) (listof char) -> (listof char)
     ;;removes characters from the two lists insofar as they match
     ;;eg (rest-letter-aux '(#\a #\b #\c) '(#\a #\b) -> '(#\c)
     (define (rest-letter-aux chars secandum)
       (cond
         [(empty? secandum) chars]
         [(char=? (first chars) (first secandum))
          (rest-letter-aux (rest chars) (rest secandum))]
         [else (error (string-append "Bug in code: second string "
                                     "must match beginning of first string."))]))}
    (list->string (rest-letter-aux my-list secandum))))
;(check-expect (rest-letter "(/abc") "bc")    
       

;; transliterate : string -> string
;; transliterate a string of letters
(define (transliterate str)
  (cond
    [(string=? str "") ""]
    [else (string-append
           (transliterate-letter-to-string (first-letter str))
           (transliterate (rest-letter str)))]))

;(check-expect 
; (transliterate "m=hnin )/aeide qe-a Phlhi/adev )Axil=how")
; "μῆνιν ἄειδε θεὰ Πηληιάδεω Ἀχιλῆος")

;;Get command line arguments. If none, give error. Else, transliterate.
;;Get command line arguments. If none, give error. Else, transliterate.
(define args (current-command-line-arguments))
(cond
  [(= (vector-length args) 0)
   (display
    (string-append
    "Please give text to translate as an argument, "
    "contained in quotation marks. " nl
    "Run 'greekify --help' for help." nl))]
  [(string=? (vector-ref args 0) "--help") (display help)]
  [(string=? (vector-ref args 0) "-o")
   (display (string-append (transliterate (vector-ref args 1))))]
  [else
   (display (string-append
             (transliterate (vector-ref args 0))
             nl))])