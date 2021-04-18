(ns ^:figwheel-hooks emh.boids
  (:require
   [goog.dom :as gdom]
   [goog.style :as gstyle]))

(def POPULATION_SIZE 50)
(def MIN_VELOCITY 10)
(def MAX_VELOCITY 20)
(def RADIUS_OF_INFLUENCE 100)
(def CLOSENESS_THRESHOLD 10)
(def FIELD_OF_VIEW (/ Math.PI 2))

(def velocity-range [MIN_VELOCITY MAX_VELOCITY])

(defn get-app-element []
  (gdom/getElement "app"))

(defn create-canvas []
  (let [canvas (gdom/createElement "canvas")
        app (get-app-element)]
    (gdom/removeChildren app)
    (gdom/appendChild app canvas)))

(defn get-canvas []
  (gdom/getFirstElementChild (get-app-element)))

(defn height []
  (let [bounds (gstyle/getBounds (get-app-element))]
    (.-height bounds)))

(defn width []
  (let [bounds (gstyle/getBounds (get-app-element))]
    (.-width bounds)))

(defn resize-canvas []
  (let [canvas (get-canvas)]
    (.setAttribute canvas "width" (width))
    (.setAttribute canvas "height" (height))))

(defn get-context []
  (let [canvas (get-canvas)]
    (.getContext canvas "2d")))

(defn xy [x y] {:x x :y y})
(defn xy-from-md [m d] (xy (* m (Math/cos d)) (* m (Math/sin d))))
(defn add-xy [xy1 xy2] (xy (+ (:x xy1) (:x xy2)) (+ (:y xy1) (:y xy2))))
(defn sub-xy [xy1 xy2] (xy (- (:x xy1) (:x xy2)) (- (:y xy1) (:y xy2))))
(defn div-xy [xy1 n] (xy (/ (:x xy1) n) (/ (:y xy1) n)))
(defn create-boid
  ([] (create-boid (xy 0 0) (xy 0 0)))
  ([location] (create-boid location (xy 0 0)))
  ([location velocity] {:location location :velocity velocity :delta (xy 0 0)}))
(defn random-location [] (xy (rand (width)) (rand (height))))
(defn random-velocity []
  (let [magnitude (+ (rand (- (second velocity-range) (first velocity-range))) (first velocity-range))
        heading (rand (* 2 Math/PI))]
    (xy-from-md magnitude heading)))
(defn random-boid [] (create-boid (random-location) (random-velocity)))
(defn heading [v] (let [angle (Math/atan2 (:y v) (:x v))] (if (< angle 0) (+ angle (* 2 Math.PI)) angle)))
(defn diff-angles [a1 a2]
  (let [
    sign (if (> a1 a2) 1 -1)
    angle (- a1 a2)
    K (* -1 sign Math.PI 2)]
    (if
      (< (Math.abs (+ K angle)) (Math.abs angle))
      (+ K angle)
      angle)))
(defn magnitude [v] (Math/sqrt (+ (* (:x v) (:x v)) (* (:y v) (:y v)))))
(defn in-view [alpha beta] (< (Math.abs (diff-angles alpha beta)) FIELD_OF_VIEW))
(defn in-range [b1 b2 maxd]
  (let [
    theta (heading (:velocity b1))
    delta (sub-xy (:location b2) (:location b1))
    distance (magnitude delta)
    angle (heading delta)
  ] (and (< distance maxd) (in-view theta angle))))

(defn draw-boid [boid]
  (let [location (:location boid)
        velocity (:velocity boid)
        x (:x location)
        y (:y location)
        ctx (get-context)]
    (.save ctx)
    (.translate ctx x y)
    (.rotate ctx (+ (/ Math/PI 2) (heading velocity)))
    (.beginPath ctx)
    (.moveTo ctx 0 -10)
    (.lineTo ctx 10 10)
    (.lineTo ctx 0 5)
    (.lineTo ctx -10 10)
    (.lineTo ctx 0 -10)
    (.fill ctx)
    (.restore ctx)))

(defn draw-boids [boids] (doseq [b boids] (draw-boid b)))

(defn move-boid [b] (create-boid (add-xy (:location b) (:velocity b)) (add-xy (:velocity b) (:delta b))))
(defn move-boids [boids] (map move-boid boids))

(defn avoid-boids-rule [b boids]
  (assoc b :delta
    (reduce
      (fn [acc b2]
        (if (in-range b b2 CLOSENESS_THRESHOLD)
          (sub-xy acc (sub-xy (:location b2) (:location b)))
          acc))
      (:delta b)
      (filter #(not= b %) boids))))

(defn center-of-gravity-rule [b boids]
  (let [locations (->> boids
                    (filter #(in-range b % RADIUS_OF_INFLUENCE))
                    (filter #(not= b %))
                    (map (fn [b2] (:location b2))))]
    (assoc b :delta
      (if (> (count locations) 0)
        (add-xy
          (:delta b)
          (div-xy
            (sub-xy
              (div-xy (reduce add-xy locations) (count locations))
              (:location b))
            100))
        (:delta b)))))

(defn match-velocity-rule [b boids]
  (let [velocities (->> boids
                     (filter #(in-range b % RADIUS_OF_INFLUENCE))
                     (filter #(not= b %))
                     (map (fn [b2] (:velocity b2))))]
    (assoc b :delta
      (if (> (count velocities) 0)
        (add-xy
          (:delta b)
          (div-xy
            (sub-xy
              (div-xy (reduce add-xy velocities) (count velocities))
              (:velocity b))
            8))
        (:delta b)))))

(defn avoid-edges-rule [b]
  (let [location (:location b)
        x (:x location)
        y (:y location)
        dx (if (< x 0) 10 (if (> x (width)) -10 0))
        dy (if (< y 0) 10 (if (> y (height)) -10 0))]
    (assoc b :delta (add-xy (:delta b) (xy dx dy)))))

(defn limit-velocity-rule [b]
  (let [velocity (:velocity b) delta (:delta b)
        mag (magnitude (add-xy velocity delta))
        dir (heading (add-xy velocity delta))
        lim-mag (min (max mag MAX_VELOCITY) MIN_VELOCITY)
        lim-vel (xy-from-md lim-mag dir)]
    (assoc b :delta (sub-xy lim-vel velocity))))

(defn apply-rules [boids] (map (fn [b]
  (-> b
    (center-of-gravity-rule boids)
    (avoid-boids-rule boids)
    (match-velocity-rule boids)
    (avoid-edges-rule)
    (limit-velocity-rule)
    ((fn [b] (do (js/console.log (:x (:delta b)) (:y (:delta b))) b)))
  )) boids))

(defonce app-state (atom {:boids (take POPULATION_SIZE (repeatedly random-boid))}))

(defn save-boids [boids] (swap! app-state assoc :boids boids))

(create-canvas)
(resize-canvas)

(defn tick []
  (.clearRect (get-context) 0 0 (width) (height))
  (let [boids (:boids @app-state)]
    (draw-boids boids)
    (-> boids
      (apply-rules)
      (move-boids)
      (save-boids))))

(defn run []
  (tick)
  (.requestAnimationFrame js/window run))

(run)

;; specify reload hook with ^;after-load metadata
(defn ^:after-load on-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
