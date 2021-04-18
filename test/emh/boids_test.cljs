(ns emh.boids-test
    (:require
     [cljs.test :refer-macros [deftest is testing]]
     [emh.boids :refer [xy create-boid random-location MIN_VELOCITY MAX_VELOCITY random-velocity heading draw-boids magnitude RADIUS_OF_INFLUENCE in-range in-view diff-angles avoid-boids-rule add-xy sub-xy center-of-gravity-rule div-xy]]))

(deftest xy-test
  (is (= (:x (xy 10 20)) 10))
  (is (= (:y (xy 10 20)) 20))
  (is (= (xy 10 20) {:x 10 :y 20})))

(deftest add-xy-test
  (is (= (add-xy (xy 10 10) (xy 15 25)) (xy 25 35)))
  (is (= (add-xy (xy -10 10) (xy 15 -25)) (xy 5 -15))))

(deftest sub-xy-test
  (is (= (sub-xy (xy 10 10) (xy 15 25)) (xy -5 -15)))
  (is (= (sub-xy (xy -10 10) (xy 15 -25)) (xy -25 35))))

(deftest div-xy-test
  (is (= (div-xy (xy 10 -8) 4) (xy 2.5 -2))))

(deftest create-boid-test
  (let [boid (create-boid (xy 10 20) (xy -5 5))]
    (is (= (:x (:location boid)) 10))
    (is (= (:y (:location boid)) 20))
    (is (= (:x (:velocity boid)) -5))
    (is (= (:y (:velocity boid)) 5))))

(deftest create-zero-boid
  (let [boid (create-boid)]
    (is (= (:x (:location boid)) 0))
    (is (= (:y (:location boid)) 0))
    (is (= (:x (:velocity boid)) 0))
    (is (= (:y (:velocity boid)) 0))))

(deftest create-motionless-boid
  (let [boid (create-boid (xy -33 42))]
    (is (= (:x (:location boid)) -33))
    (is (= (:y (:location boid)) 42))
    (is (= (:x (:velocity boid)) 0))
    (is (= (:y (:velocity boid)) 0))))

;; (deftest random-location-test
;;   (is (>= (:x (random-location)) 0))
;;   (is (<= (:x (random-location)) (width)))
;;   (is (>= (:y (random-location)) 0))
;;   (is (<= (:y (random-location)) (height))))

(deftest random-velocity-test
  (let [v (random-velocity)
        magnitude (Math.sqrt (+ (* (:x v) (:x v)) (* (:y v) (:y v))))]
    (is (>= magnitude (* -1 MAX_VELOCITY)))
    (is magnitude MAX_VELOCITY)
    (is (not (and
      (> magnitude (* -1 MIN_VELOCITY))
      (< magnitude MIN_VELOCITY))))))

(deftest heading-test
  (is (= (heading (xy 0 0)) 0))
  (is (= (heading (xy 10 0)) 0))
  (is (= (heading (xy 10 10)) (/ Math.PI 4)))
  (is (= (heading (xy 0 10)) (/ Math.PI 2)))
  (is (= (heading (xy -10 0)) Math.PI))
  (is (= (heading (xy -10 -10)) (* 5 (/ Math.PI 4))))
  (is (= (heading (xy 0 -10)) (* 3 (/ Math.PI 2))))
  (is (= (heading (xy 10 -10)) (* 7 (/ Math.PI 4))))
  (is (= (heading (xy -10 10)) (* 3 (/ Math.PI 4)))))

;; (deftest draw-boids-returns-boids
;;   (let [boids [{:location {:x 10, :y 10}, :velocity {:x -5, :y 5}, :delta {:x 0, :y 0}}]]
;;     (is (= (draw-boids boids) boids))))

(deftest magnitude-test
  (is (= (magnitude (xy 0 0)) 0))
  (is (= (magnitude (xy 3 4)) 5))
  (is (= (magnitude (xy -3 -4)) 5))
  (is (= (magnitude (xy -3 4)) 5))
  (is (= (magnitude (xy 3 -4)) 5)))

(def west (heading (xy -10 0)))
(def southwest (heading (xy -10 10)))
(def south (heading (xy 0 10)))
(def southeast (heading (xy 10 10)))
(def east (heading (xy 10 0)))
(def northeast (heading (xy 10 -10)))
(def north (heading (xy 0 -10)))
(def northwest (heading (xy -10 -10)))

(deftest diff-angles-test
  (is (= (diff-angles west west) 0))
  (is (= (diff-angles west northwest) (* Math.PI (/ -1 4))))
  (is (= (diff-angles west north) (* Math.PI (/ -1 2))))
  (is (= (diff-angles west northeast) (* Math.PI (/ -3 4))))
  (is (= (diff-angles west east) Math.PI))
  (is (= (diff-angles west southeast) (* Math.PI (/ 3 4))))
  (is (= (diff-angles west south) (* Math.PI (/ 1 2))))
  (is (= (diff-angles west southwest) (* Math.PI (/ 1 4))))
  (is (= (diff-angles northwest west) (* Math.PI (/ 1 4))))
  (is (= (diff-angles north west) (* Math.PI (/ 1 2))))
  (is (= (diff-angles northeast west) (* Math.PI (/ 3 4))))
  (is (= (diff-angles east west) (* -1 Math.PI)))
  (is (= (diff-angles southeast west) (* Math.PI (/ -3 4))))
  (is (= (diff-angles south west) (* Math.PI (/ -1 2))))
  (is (= (diff-angles southwest west) (* Math.PI (/ -1 4))))
)

(deftest in-view-test-angles
  (is (= (in-view west northwest) true))
  (is (= (in-view west west) true))
  (is (= (in-view west southwest) true))
  (is (= (in-view west south) false))
  (is (= (in-view west southeast) false))
  (is (= (in-view west east) false))
  (is (= (in-view west northeast) false))
  (is (= (in-view west north) false))
  (is (= (in-view east northwest) false))
  (is (= (in-view east west) false))
  (is (= (in-view east southwest) false))
  (is (= (in-view north northwest) true))
  (is (= (in-view north west) false))
  (is (= (in-view north (+ west 0.01)) true))
  (is (= (in-view north (- west 0.01)) false))
  (is (= (in-view north (+ east 0.01)) false))
  (is (= (in-view north (- east 0.01)) true))
  (is (= (in-view north southwest) false))
  (is (= (in-view north south) false))
  (is (= (in-view east west) false))
  (is (= (in-view northeast east) true))
)

(deftest in-range-test-overlap
  (is (= (in-range (create-boid) (create-boid) 100) true)))

(deftest in-range-test-far-away
  (is (= (in-range (create-boid) (create-boid (xy RADIUS_OF_INFLUENCE RADIUS_OF_INFLUENCE)) 100) false))
  (is (= (in-range (create-boid) (create-boid (xy (* -1 RADIUS_OF_INFLUENCE) RADIUS_OF_INFLUENCE)) 100) false))
  (is (= (in-range (create-boid) (create-boid (xy (* -1 RADIUS_OF_INFLUENCE) (* -1 RADIUS_OF_INFLUENCE))) 100) false))
  (is (= (in-range (create-boid) (create-boid (xy RADIUS_OF_INFLUENCE (* -1 RADIUS_OF_INFLUENCE))) 100) false)))

(deftest in-range-test-in-view
  (is (= (in-range (create-boid) (create-boid 0 10) 100) true))
  (is (= (in-range (create-boid) (create-boid -10 10) 100) true))
  (is (= (in-range (create-boid) (create-boid 10 10) 100) true)))

(deftest in-range-test-not-in-view
  (is (= (in-range (create-boid) (create-boid (xy -10 -10)) 100) false))
  (is (= (in-range (create-boid) (create-boid (xy -10 0)) 100) false))
  (is (= (in-range (create-boid) (create-boid (xy -10 10)) 100) false))
  (is (= (in-range (create-boid) (create-boid (xy 0 -10)) 100) false))
  (is (= (in-range (create-boid) (create-boid (xy 0 10)) 100) false)))

(deftest avoid-boids-rule-test-2-boids
  (let [b1 (create-boid (xy 10 10))
        b2 (create-boid (xy 15 10))]
    (is (= (in-range b1 b2 10) true))
    (is (= (:delta (avoid-boids-rule b1 [b1 b2])) (xy -5 0)))
    (is (= (:delta (avoid-boids-rule b2 [b1 b2])) (xy 0 0)))))

(deftest avoid-boids-rule-test-3-boids
  (let [b1 (create-boid (xy 10 10))
        b2 (create-boid (xy 19 10))
        b3 (create-boid (xy 15 15))]
    (is (= (in-range b1 b2 100) true))
    (is (= (in-range b1 b3 100) true))
    (is (= (:delta (avoid-boids-rule b1 [b1 b2 b3])) (xy -14 -5)))))

(deftest center-of-gravity-rule-test
  (let [b1 (create-boid (xy 10 10))
        b2 (create-boid (xy 20 10))
        b3 (create-boid (xy 200 10))
        b4 (create-boid (xy 15 15))]
    (is (= (:delta (center-of-gravity-rule b1 [b1 b2 b3 b4])) (xy 0.075 0.025)))))

(deftest center-of-gravity-rule-test-nothing-in-range
  (let [b1 (create-boid)
        b2 (create-boid (xy 200 200))
        b3 (create-boid (xy -200 -200))
        b4 (create-boid (xy 150 -150))]
    (is (= (:delta (center-of-gravity-rule b1 [b1 b2 b3 b4])) (xy 0 0)))))
