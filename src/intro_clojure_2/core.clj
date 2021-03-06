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
                                      [:cool]]}
                       :cookies {:ingredients {:egg 1
                                               :flour 1
                                               :butter 1
                                               :sugar 1}
                                 :steps [[:add :all]
                                         [:mix]
                                         [:pour]
                                         [:bake 30]
                                         [:cool]]}
                       :brownies {:ingredients {:egg 2
                                               :flour 2
                                               :butter 2
                                               :cocoa 2
                                               :sugar 1
                                               :milk 1}
                                  :steps [[:add :butter]
                                          [:add :cocoa]
                                          [:add :sugar]
                                          [:mix]
                                          [:add :egg]
                                          [:add :flour]
                                          [:add :milk]
                                          [:mix]
                                          [:pour]
                                          [:cool]]}}
             :ingredients {:egg {:storage :fridge
                                 :usage :squeezed}
                           :milk {:storage :fridge
                                  :usage :scooped}
                           :flour {:storage :pantry
                                   :usage :scooped}
                           :butter {:storage :fridge
                                    :usage :simple}
                           :sugar {:storage :pantry
                                   :usage :scooped}
                           :cocoa {:storage :pantry
                                   :usage :scooped}}})

(def usage {:squeezed (fn [ingredient amount]
                        (dotimes [i amount]
                          (grab ingredient)
                          (squeeze)
                          (add-to-bowl)))
            :simple (fn [ingredient amount]
                      (dotimes [i amount]
                        (grab ingredient)
                        (add-to-bowl)))
            :scooped (fn [ingredient amount]
                       (grab :cup)
                       (dotimes [i amount]
                         (scoop ingredient)
                         (add-to-bowl))
                       (release))})

(defn error [& args]
  (apply println [args])
  :error)

(defn add
  ([ingredient]
     (add ingredient 1))
  ([ingredient amount]
   (let [u (:usage (ingredient (:ingredients baking)))
         f (usage u)]
     (f ingredient amount))))

(def actions {:add (fn
                     ([recipe ingredient]
                      (cond
                        (= :all ingredient)
                        (doseq [[ingredient amount] (:ingredients recipe)]
                          (add ingredient amount))
                        (contains? (:ingredients recipe) ingredient)
                        (add ingredient (get (:ingredients recipe) ingredient))))
                      ([recipe ingredient amount]
                       (add ingredient amount)))
              :mix (fn [recipe]
                     (mix))
              :pour (fn [recipe]
                      (pour-into-pan))
              :bake (fn [recipe minutes]
                      (bake-pan minutes))
              :cool (fn [recipe]
                      (cool-pan))})

(defn perform [recipe step]
  (let [f (actions (first step))]
    (apply f recipe (rest step))))

(defn bake-recipe [recipe]
  (last
   (for [step (:steps recipe)]
     (perform recipe step))))

(def scooped-ingredients #{:flour :sugar :milk :cocoa})

(defn scooped? [ingredient]
  (contains? scooped-ingredients ingredient))

(def squeezed-ingredients #{:egg})

(defn squeezed? [ingredient]
  (contains? squeezed-ingredients ingredient))

(def simple-ingredients #{:butter})

(defn simple? [ingredient]
  (contains? simple-ingredients ingredient))

(defn add-ingredients [a b]
  (merge-with + a b))

(defn multiply-ingredients [n ingredients]
  (into {}
        (for [[ingredient amount] ingredients]
          [ingredient (* amount n)])))

(defn order->ingredients [order]
  (reduce add-ingredients
          (for [[item amount] (:items order)]
            (multiply-ingredients amount
                                  (:ingredients (item (:recipes baking)))))))

(defn orders->ingredients [orders]
  (reduce add-ingredients (map order->ingredients orders)))

(defn load-up-amount [ingredient amount]
  (dotimes [i amount]
    (load-up ingredient)))

(defn unload-amount [ingredient amount]
  (dotimes [i amount]
    (unload ingredient)))
         
(defn fetch-ingredient
  ([ingredient]
   (fetch-ingredient ingredient 1))
  ([ingredient amount]
   (go-to (:storage (ingredient (:ingrediants baking))))
   (load-up-amount ingredient amount)
   (go-to :prep-area)
   (unload-amount ingredient amount)))

(defn fetch-list [shopping-list]
  (let [with-storage (for [[ingredient amount] shopping-list]
                       {:ingredient ingredient
                        :amount amount
                        :storage (:storage (ingredient (:ingredients baking)))})]
    (doseq [[location ingredients] (group-by :storage with-storage)]
      (go-to location)
      (doseq [ingredient ingredients]
        (load-up-amount (:ingredient ingredient) (:amount ingredient))))
    (go-to :prep-area)
    (doseq [[ingredient amount] shopping-list]
      (unload-amount ingredient amount))))

(defn bake [item]
  (bake-recipe ((:recipes baking) item)))

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
  (day-at-the-bakery))
