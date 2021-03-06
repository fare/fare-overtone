;(in-ns 'fare-overtone.core)

(ns fare-overtone.core
  (:require
   [clojure.math.numeric-tower :as math]
   [overtone.live :as live]
   ;;[overtone.inst.piano :as piano]
   )
  (:use
   [overtone.synth.stringed]
   [overtone.live]))

;;; Non-intrusive print-debugging
(defmacro DBG [tag & exprs]
    "debug macro for print-debugging:
tag is typically a constant string or keyword to identify who is printing,
but can be an arbitrary expression returning a tag to be princ'ed first;
if the tag evaluates to falsy (false or nil), nothing is printed.
exprs are expressions, which when the TAG was truthy are evaluated in order,
with their source code then their return values being printed each time.
The last expresion is *always* evaluated and its multiple values are returned,
but its source and return values are only printed if the tag was truthy;
previous expressions are not evaluated at all if TAG was falsy.
The macro expansion has relatively low overhead in space or time."
  (let [last-expr (last exprs)
        other-exprs (butlast exprs)
        thunk (gensym "thunk_")]
    `(let [tag# ~tag]
       (letfn ~(if exprs `[(~'thunk [] ~last-expr)] [])
         (if tag#
             (DBG-helper tag#
                         [~@(map #(do `['~% (fn [] ~%)]) other-exprs)]
                         ~(when exprs `['~last-expr ~'thunk]))
             ~(if exprs `(~'thunk) nil))))))

(defn DBG-helper [tag xts last-xt]
  ;; Helper for the above debugging macro
  (letfn [(foo [[expression thunk]]
            (print "  ") (pr expression) (print " => ")
            (let [val (thunk)]
              (prn val)
              val))]
    (println tag)
    (doseq [xt xts] (foo xt))
    (when last-xt (foo last-xt))))

(definst foo [freq 440 volume 0.8] (* volume (saw freq)))
;(foo)
;(ctl foo :freq 660 :volume 0.1)
;(kill foo)


(defn midifreq [midi] ; a4 = 69
  (* 440 (math/expt 2 (/ (- midi 69) 12))))

(defn foon [midi volume] (foo (midifreq midi) volume))
;(foon 57 0.8)
;(foon 61 0.9)
;(foon 64 1.0)
;(kill foo)


(definst quux [freq 440] (* 0.3 (saw freq)))
; (quux)
; (ctl quux :freq 660)
; (kill quux)

(definst trem [freq 440 depth 10 rate 6 length 3]
    (* 0.3
       (line:kr 0 1 length FREE)
       (saw (+ freq (* depth (sin-osc:kr rate))))))

;(trem)
;(ctl trem :length 10)

(definst kick [freq 120 dur 0.3 width 0.5]
  (let [freq-env (* freq (env-gen (perc 0 (* 0.99 dur))))
        env (env-gen (perc 0.01 dur) 1 1 0 1 FREE)
        sqr (* (env-gen (perc 0 0.01)) (pulse (* 2 freq) width))
        src (sin-osc freq-env)
        drum (+ sqr (* env src))]
    (compander drum drum 0.2 1 0.1 0.01 0.01)))

;(kick)

(definst c-hat [amp 0.8 t 0.04]
  (let [env (env-gen (perc 0.001 t) 1 1 0 1 FREE)
        noise (white-noise)
        sqr (* (env-gen (perc 0.01 0.04)) (pulse 880 0.2))
        filt (bpf (+ sqr noise) 9000 0.5)]
    (* amp env filt)))

;(c-hat)

(definst piano [freq 440
                gate 1
                vel 100
                decay 0.8
                release 0.8
                hard 0.8
                velhard 0.8
                muffle 0.8
                velmuff 0.8
                velcurve 0.8
                stereo 0.2
                tune 0.5
                random 0.1
                stretch 0.1
                sustain 0.1]
  (let [snd (mda-piano {:freq freq
                        :gate gate
                        :vel vel
                        :decay decay
                        :release release
                        :hard hard
                        :velhard velhard
                        :muffle muffle
                        :velmuff velmuff
                        :velcurve velcurve
                        :stereo stereo
                        :tune tune
                        :random random
                        :stretch stretch
                        :sustain sustain})]
    (detect-silence snd 0.005 :action FREE)
    (* 1 snd))) ;;TODO: figure out why this mul is required


(defn play-note-or-notes [n-or-ns]
  (if (sequential? n-or-ns)
    (doseq [n n-or-ns] (piano (midifreq (note n))))
    (play-note-or-notes (list n-or-ns))))

(defn play-phrase
  [start step phrase]
  (dorun
   (map-indexed
    (fn [i n-or-ns]
      (at (+ start (* i step)) (play-note-or-notes n-or-ns)))
    phrase)))

(defn play-phrase2
  [start base-duration phrase]
  (loop [time start
         notes phrase]
    (when (seq notes)
      (let [[[note duration] & more] notes]
        (println (format "%s" [note duration]))
        (at time (play-note-or-notes note))
        (recur (+ time (* base-duration duration)) more)))))

(def happy-birthday
  [[[:c4 :e4 :g4] 3/16][[:c4 :e4 :g4] 1/16] [[:c4 :e4 :a4] 1/4] [[:c4 :e4 :g4] 1/2]
   [[:e4 :g4 :c5] 1/4] [[:d4 :f4 :g4 :b4] 3/4]
   [[:b3 :d4 :g4] 3/16][[:b3 :d4 :g4] 1/16] [[:d4 :f4 :a4] 1/4] [[:b3 :d4 :g4] 1/2]
   [[:f4 :g4 :b4 :d5] 1/4] [[:e4 :g4 :c4] 3/4]
   [[:c4 :e4 :g4] 3/16][[:c4 :e4 :g4] 1/16] [[:c5 :e5 :g5] 1/4] [[:g4 :c4 :e5] 1/2]
   [[:e4 :g4 :c5] 1/4] [[:d4 :f4 :g4 :b4] 3/16] [[:d4 :f4 :a4] 1/16] [[:d4 :f4 :a4] 3/4] ;; (fermata)
   [[:b4 :d5 :f5] 3/16] [[:b4 :d5 :f5] 1/16] [[:g4 :c4 :e5] 1/4] [[:e4 :g4 :c5] 1/2]
   [[:f4 :g4 :b4 :d5] 1/4] [[:e4 :g4 :c4] 3/4]
   ])

(def string-notes [:e2 :a2 :d3 :g3 :b3 :e4])

(defn chord-to-fingers [notes]
  ;; Play the last string that can play a note.
  ;; Don't care for human feasibility of the fingering for now;
  ;; the computer has perfect fingers.
  (loop [notes-midi (reverse (map note notes))
         string-midi (reverse (map note string-notes))
         fingers '()]
    (cond
      (empty? notes-midi) (vec (concat (map #(do % -1) string-midi) fingers))
      (empty? string-midi) (throw (ex-info "Can't play notes" {:notes notes}))
      :else (let [[n & nm] notes-midi
                  [s & sm] string-midi]
              (if (>= n s)
                (recur nm sm (cons (- n s) fingers))
                (recur notes-midi sm (cons -1 fingers)))))))

(defn pp [x] (play-phrase2 (now) (* 4 60 1.0e3 1/180) x))

;;(pp happy-birthday)
;; annemoroney@alum.mit.edu


;; twinkle twinkle little star
(def phrase1 [:c3 :c3 :g3 :g3 :a3 :a3 :g3])

;; how I wonder what you are
(def phrase2 '[(:a2 :f3) (:a2 :f3) (:g2 :e3) (:g2 :e3) (:b2 :d3) (:b2 :d3) (:e2 :g2 :c3)])

(def phrase3 '[(:c3 :e3 :g3) (:c3 :e3 :g3) (:b2 :d3 :f3) (:b2 :d3 :f3)
               (:g2 :c3 :e3) (:g2 :c3 :e3) (:g2 :b2 :d3)])

(def phrase1-2 '[(:e2 :g2 :c3) (:e2 :g2 :c3) (:c3 :e3 :g3) (:c3 :e3 :g3)
                 (:c3 :f3 :a3) (:c3 :f3 :a3) (:c3 :e3 :g3)])

(def phrase2-2 '[(:b2 :d3 :f3) (:b2 :d3 :f3) (:g2 :c3 :e3) (:g2 :c3 :e3)
                 (:g2 :b2 :d3) (:g2 :b2 :d3) (:e2 :g2 :c3)])

(def phrases (list phrase1 phrase2 phrase3 phrase3 phrase1-2 phrase2-2))

(defn twinkle
  "Plays the first part of Twinkle Twinkle Little Star. With no args,
  starts immediately, or you can provide an optional start argument."
  ([]
   (twinkle (now)))
  ([start]
   (let [step 650]
     (loop [start start phrases phrases]
       (if (seq phrases)
         (let [[phrase & more-phrases] phrases]
           (play-phrase start step phrase)
           (recur (+ start (* step (inc (count phrase)))) more-phrases)))))))
;(twinkle)
;(kill piano)
;piano/piano

(def g (guitar))
(ctl g :pre-amp 80.0 :distort 0.1)

(defn strum-chord
  ([time chord direction interval]
   (cond
     (every? integer? chord) (guitar-strum g chord direction interval time)
     (every? keyword? chord) (strum-chord time (chord-to-fingers chord) direction interval)
     :else (throw (ex-info "bad chord" {:chord chord}))))
  ([time chord direction] (strum-chord time chord direction 0.1))
  ([time chord] (strum-chord time chord :down 0.1)))

(defn play-phrase-g1
  [start base-duration phrase]
  (loop [time start
         notes phrase]
    (when (seq notes)
      (let [[[strums duration] & more] notes]
        (strum-chord time strums)
        (recur (+ time (* base-duration duration)) more)))))

(defn gg [x] (play-phrase-g1 (now) (* 4 60 1.0e3 1/90) x))


(do
(def c4-c [:c3 :e3 :g3 :c4])
(def g3-c [:g2 :c3 :e3 :g3])
(def e3-c [:g2 :c3 :e3]) ;; :e2
(def f3-g7 [:b2 :d3 :f3]) ;; :g2
(def b3-g7 [:d3 :f3 :g3 :b3])
(def bes3-bes [:bb2 :d3 :f3 :bb3])
(def d3-bes [:f2 :bb2 :d3])
(def d4-bes [:d3 :f3 :bb3 :d4])
(def f3-bes [:bb2 :d3 :f3])
(def ees3-ees [:g2 :bb2 :eb3])
(def c4-bes9 [:bb2 :d3 :f3 :a3 :c4]) ; XXX ???
(def bes3-bes [:bb2 :d3 :f3 :bb3])
(def f4-f [:f3 :a3 :c4 :f4])


(def g4-c [:g3 :c4 :e4 :g4])
(def e4-c [:e3 :g3 :c4 :e4])
(def a4-am7 [:g3 :c4 :e4 :a4])
(def c5-c [:c4 :e4 :g4 :c5])
(def b5-c7 [:c4 :e4 :g4 :b5])
(def a4-f [:a3 :c4 :f4 :a4])
(def f4-g7 [:g3 :b3 :d4 :f4])
(def d4-g [:d3 :g3 :b3 :d4])
(def g4-g [:g3 :b3 :d4 :g4])
(def d5-bm [:d4 :f4 :b4 :d5])
(def c5-dm7 [:d4 :f4 :a4 :c5])
(def b4-g7 [:d4 :f4 :g4 :b4])
(def a4-bm7 [:b3 :d4 :f4 :a4])
(def a4-am7 [:g3 :c4 :e4 :a4])
(def e5-c#dim [:e4 :g4 :c#5 :e5])
(def d5-edim7 [:e4 :g4 :bb4 :d5])
(def c#5-a7 [:e4 :g4 :a4 :c#5])
(def bb4-c#dim7- [:c#4 :e4 :g4 :bb4])
(def bb4-bb7 [:a3 :d4 :f4 :bb4])
(def a4-dm [:a3 :d4 :f4 :a4])
(def d5-dm [:d4 :f4 :a4 :d5])
(def f5-dm [:d4 :f4 :a4 :f5])
(def d5-m [:d4 :f4 :a4 :d5])
(def e5-c [:e4 :g4 :c4 :e5])
(def d5-g [:d4 :g4 :b4 :d5])
(def g4-g7 [:b3 :d4 :f4 :g4])
(def f5-g7 [:g4 :b4 :d5 :f5])
(def d5-c9x7 [:c4 :e4 :g4 :d5])
(def e5-e9x7 [:f4 :g4 :b4 :e5])

(def rocky-sabaki ;; http://fare.livejournal.com/166932.html
  [[g4-c 3/8][e4-c 1/4][a4-am7 1/8][g4-c 3/8][e4-c 1/4][g4-c 1/8]
     [c5-c 1/4][b5-c7 1/8][a4-f 1/4][g4-c 1/8][f4-g7 3/8][d4-g 3/8]
   [f4-g7 3/8][d4-g 1/4][g4-g 1/8][f4-g7 3/8][d4-g 1/4][f4-g7 1/8]
     [d5-bm 1/4][c5-dm7 1/8][b4-g7 1/4][a4-bm7 1/8][a4-am7 1/4][g4-c 1/8][e4-c 3/8]
   [g4-c 3/8][e4-c 1/4][a4-am7 1/8][g4-c 3/8][e4-c 1/4][g4-c 1/8]
     [e5-c#dim 1/4][d5-edim7 1/8][c#5-a7 1/4][bb4-c#dim7- 1/8][bb4-bb7 1/4][a4-dm 1/8][d5-dm 3/8]
   [f5-dm 3/8][d5-m 1/4][f5-dm 1/8][e5-c 3/8][c5-c 1/4][e5-c 1/8]
     [d5-g 3/8][g4-g7 1/4][f5-g7 1/8][e5-c 3/8][c5-c 1/8][d5-c9x7 1/8][e5-c 1/8]
   [f5-dm 3/8][d5-m 1/4][f5-dm 1/8][e5-c 3/8][c5-c 1/4][e5-c 1/8]
     [d5-g 3/8][g4-g7 (+ 1/4 1/8)][e5-e9x7 (+ 1/4 1/2)][d5-bm 1/8][c5-c (+ 5/8 6/8)]
]))

;(gg rocky-sabaki)


(def leonora-s-song
  [;; bar 1
   [g4-c 1/8][g4-c 1/16][f4-g7 1/16]
   [e4-c 1/8][d4-g 1/16][f4-g7 1/16]
   [e4-c 1/8][c4-c 1/8]
   [g3-c 1/8][e3-c 1/16][f3-g7 1/16]
   ;; bar 2
   [g3-c 1/8][g3-c 1/16][b3-g7 1/16]
   [c4-c 1/8][c4-c 1/16][e4-c 1/16]
   [f4-g7 1/4]
   [d4-g 1/4]
   ;; bar 3
   [f4-g7 1/8][f4-g7 1/16][e4-c 1/16]
   [d4-bes 1/8][bes3-bes 1/16][c4-bes9 1/16]
   [d4-bes 1/8][bes3-bes 1/8]
   [f3-bes 1/8][d3-bes 1/16][ees3-ees 1/16]
   ;; bar 4
   [[:bb2 :d3 :f3] 1/8][[:f2 :f3] 1/16][[:f3 :a3] 1/16]
   [[:bb3] 1/8][[:d4] 1/16][[:f4] 1/16]
   [[:e4] 1/4]
   [[:c#4] 1/4]
   ;; bar 5
   [g4-c 1/8][g4-c 1/16][f4-g7 1/16]
   [e4-c 1/8][d4-g 1/16][f4-g7 1/16]
   [e4-c 1/8][c4-c 1/8]
   [g3-c 1/8][e3-c 1/16][f3-g7 1/16]
   ;; bar 6
   [g3-c 1/8][g3-c 1/16][b3-g7 1/16]
   [c4-c 1/8][c4-c 1/16][e4-c 1/16]
   [a4-f 1/4]
   [f4-f 1/4]
   ])
