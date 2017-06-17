(ns intro-clojure-2.core
  [:use bakery.core])

(def baking {:recipes {:cake {:ingredients {:egg 2
                                            :flour 2
                                            :sugar 1
                                            :milk 1}
                              :steps [[:add :all]
                                      [:mix]
                                      [:pour]
                                      [:bake 25]
                                      [:cool]]}}})

(defn error [& args]
  (apply println [args])
  :error)

(defn scooped? [ingredient]
  (cond
   (= ingredient :milk)
   true
   (= ingredient :flour)
   true
   (= ingredient :sugar)
   true
   :else
   false))

(defn squeezed? [ingredient]
  (= ingredient :egg))

(defn simple? [ingredient]
  (= ingredient :butter))

(defn add-squeezed
  ([ingredient]
     (add-squeezed ingredient 1))
  ([ingredient amount]
     (if (squeezed? ingredient)
       (dotimes [i amount]
         (grab ingredient)
         (squeeze)
         (add-to-bowl))
       (do
         (println "This function only works on squeezed ingredients. You asked me to squeeze" ingredient)
         :error))))

(defn add-scooped
  ([ingredient]
     (add-scooped ingredient 1))
  ([ingredient amount]
     (if (scooped? ingredient)
       (do
         (grab :cup)
         (dotimes [i amount]
          (scoop ingredient)
          (add-to-bowl))
         (release))
       (error "This function only works on scooped ingredients. You asked me to scoop" ingredient)
       )))

(defn add-simple
  ([ingredient]
     (add-simple ingredient 1))
  ([ingredient amount]
     (if (simple? ingredient)
       (dotimes [i amount]
         (grab ingredient)
         (add-to-bowl))
       (error "This function only works on simple ingredients. You asked me to add" ingredient)
       )))

(defn add
  ([ingredient]
     (add ingredient 1))
  ([ingredient amount]
     (cond
      (squeezed? ingredient)
      (add-squeezed ingredient amount)

      (simple? ingredient)
      (add-simple ingredient amount)

      (scooped? ingredient)
      (add-scooped ingredient amount)

      :else
      (error "I do not have the ingredient" ingredient)
      )))

(defn perform [recipe step]
  (cond
    (= :cool (first step))
    (cool-pan)
    (= :mix (first step))
    (mix)
    (= :pour (first step))
    (pour-into-pan)
    (= :bake (first step))
    (bake-pan (second step))
    (= :add (first step))
    (cond
      (= [:all] (rest step))
      (doseq [[ingredient amount] (:ingredients recipe)]
        (add ingredient amount))
      (= 2 (count (rest step)))
      (apply add (rest step))
      (contains? (:ingredients recipe) (second step))
      (add (second step) ((:ingredients recipe) (second step)))
      :else
      (error "This recipe does not call for" (first step)))
    :else
    (error "I do not know how to" (first step))))

(defn bake-cake []
  (add :egg 2)
  (add :flour 2)
  (add :milk 1)
  (add :sugar 1)

  (mix)

  (pour-into-pan)
  (bake-pan 25)
  (cool-pan))

(defn bake-cookies []
  (add :egg 1)
  (add :flour 1)
  (add :butter 1)
  (add :sugar 1)

  (mix)

  (pour-into-pan)
  (bake-pan 30)
  (cool-pan))

(defn bake-brownies []
  (add :butter 2)
  (add :sugar 1)
  (add :cocoa 2)

  (mix)

  (add :egg 2)
  (add :flour 2)
  (add :milk 1)

  (mix)

  (pour-into-pan)
  (bake-pan 35)
  (cool-pan))

(def fridge-ingredients #{:milk :egg :butter})

(defn from-fridge? [ingredient]
  (contains? fridge-ingredients ingredient))

(def pantry-ingredients #{:flour :sugar :cocoa})

(defn from-pantry? [ingredient]
  (contains? pantry-ingredients ingredient))

(def scooped-ingredients #{:flour :sugar :milk :cocoa})

(defn scooped? [ingredient]
  (contains? scooped-ingredients ingredient))

(def squeezed-ingredients #{:egg})

(defn squeezed? [ingredient]
  (contains? squeezed-ingredients ingredient))

(def simple-ingredients #{:butter})

(defn simple? [ingredient]
  (contains? simple-ingredients ingredient))

(defn fetch-from-pantry
  ([ingredient]
   (fetch-from-pantry ingredient 1))
  ([ingredient amount]
   (if (from-pantry? ingredient)
     (do
       (go-to :pantry)
       (dotimes [i amount]
         (load-up ingredient))
       (go-to :prep-area)
       (dotimes [i amount]
         (unload ingredient)))
     (error "This function only works on ingredients that are stored in the pantry. You asked me to fetch" ingredient))))
         
(defn fetch-from-fridge
  ([ingredient]
   (fetch-from-fridge ingredient 1))
  ([ingredient amount]
   (if (from-fridge? ingredient)
     (do
       (go-to :fridge)
       (dotimes [i amount]
         (load-up ingredient))
       (go-to :prep-area)
       (dotimes [i amount]
         (unload ingredient)))
     (error "This function only works on ingredients that are stored in the fridge. You asked me to fetch" ingredient))))

(defn fetch-ingredient
  ([ingredient]
   (fetch-ingredient ingredient 1))
  ([ingredient amount]
   (cond
     (from-fridge? ingredient)
     (fetch-from-fridge ingredient amount)
     (from-pantry? ingredient)
     (fetch-from-pantry ingredient amount)
     :else
     (error "This function only works on ingredients found in either the pantry or the fridge.  You asked me to fetch" ingredient))))

(defn add-ingredients [a b]
  (merge-with + a b))

(defn multiply-ingredients [n ingredients]
  (into {}
        (for [[ingredient amount] ingredients]
          [ingredient (* amount n)])))

(defn order->ingredients [order]
  (add-ingredients
   (multiply-ingredients (:cake (:items order) 0) {:egg 2
                                                   :flour 2
                                                   :sugar 1
                                                   :milk 1})
   (multiply-ingredients (:cookies (:items order) 0) {:egg 1
                                                      :flour 1
                                                      :sugar 1
                                                      :butter 1})
   (multiply-ingredients (:brownies (:items order) 0) {:egg 2
                                                       :flour 2
                                                       :cocoa 2
                                                       :sugar 1
                                                       :butter 2
                                                       :milk 1})))

(defn orders->ingredients [orders]
  (reduce add-ingredients (map order->ingredients orders)))

(defn load-up-amount [ingredient amount]
  (dotimes [i amount]
    (load-up ingredient)))

(defn unload-amount [ingredient amount]
  (dotimes [i amount]
    (unload ingredient)))
         
(defn fetch-list [shopping-list]
  (doseq [[location ingredients] {:pantry pantry-ingredients
                                  :fridge fridge-ingredients}]
    (go-to location)
    (doseq [ingredient ingredients]
      (load-up-amount ingredient (ingredient shopping-list 0))))

  (go-to :prep-area)
  (doseq [[ingredient amount] shopping-list]
    (unload-amount ingredient amount)))

(defn bake [item]
  (cond
    (= :cake item)
    (bake-cake)
    (= :cookies item)
    (bake-cookies)
    (= :brownies item)
    (bake-brownies)
    :else
    (error "I don't know how to bake" item)))

(defn day-at-the-bakery []
  (let [orders (get-morning-orders)
        ingredient-list (orders->ingredients orders)]
    (fetch-list ingredient-list)
    (doseq [order orders]
      (let [items (:items order)
            racks (for [[item amount] items
                        i (range amount)]
                    (bake item))
            receipt {:orderid (:orderid order)
                     :address (:address order)
                     :rackids racks}]
        (delivery receipt)))))
              
(defn -main []
  (day-at-the-bakery)(status))
