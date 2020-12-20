(ns t.day20)

(defn rev
  [s]
  (apply str (reverse s)))

(defn parse
  [lines]
  (->> lines
       (partition-by #{""})
       (remove #{[""]})
       (map (fn [tile]
              (let [[_ id] (re-matches #"Tile (\d+):" (first tile))]
                {:id (Long/parseLong id)
                 :borders [(second tile)
                           (apply str (rest (map last tile)))
                           (last tile)
                           (apply str (rest (map first tile)))]
                 :content (->> (rest tile)
                               rest butlast
                               (map (fn [line] (apply str (rest (butlast line))))))})))))

(defn part1
  [input]
  (let [once (->> input
                 (mapcat :borders)
                 (mapcat (fn [x] [x (rev x)]))
                 frequencies
                 (filter (fn [[val count]] (= count 1)))
                 (map first)
                 set)]
    (->> input
         (filter (fn [{:keys [borders]}]
                   (->> borders
                        (filter once)
                        count
                        (= 2))))
         (map :id)
         (reduce *))))

(defn part2
  [input]
  (let [rotate-image (fn [image]
                       (->> image
                            (apply map vector)
                            (mapv rev)))
        flip-image (fn [image] (vec (reverse image)))
        rotate-tile (fn [{id :id, content :content, [top right bottom left] :borders}]
                      {:id id
                       :borders [(rev left) top (rev right) bottom]
                       :content (rotate-image content)})
        flip-tile (fn [{id :id, content :content, [top right bottom left] :borders}]
                    {:id id
                     :borders [bottom (rev right) top (rev left)]
                     :content (flip-image content)})
        all-tile-variants (fn [p]
                            (->> [p (flip-tile p)]
                                 (mapcat (fn [p] (take 4 (iterate rotate-tile p))))))
        is-unique (->> input
                       (mapcat :borders)
                       (mapcat (fn [x] [x (rev x)]))
                       frequencies
                       (filter (fn [[val count]] (= count 1)))
                       (map first)
                       set)
        make-top-left (fn [c]
                        (if (and (is-unique (first (:borders c)))
                                 (is-unique (last (:borders c))))
                          c
                          (recur (rotate-tile c))))
        start-corner (->> input
                          (filter (fn [{:keys [borders]}]
                                    (->> borders
                                         (filter is-unique)
                                         count
                                         (= 2))))
                          first
                          make-top-left)
        side (long (Math/sqrt (count input)))
        image (loop [placed [[start-corner]]
                     jumble (->> input
                                 (remove (fn [t] (= (:id t) (:id start-corner))))
                                 (mapcat all-tile-variants))
                     n 1]
                (if (empty? jumble)
                  (->> placed
                       (mapcat (fn [tile-line]
                                 (->> tile-line
                                      (map :content)
                                      (apply map vector)
                                      (map #(apply str %)))))
                       vec)
                  (let [x (rem n side)
                        y (quot n side)
                        match-above (if (= 0 y)
                                      (fn [p]
                                        (is-unique (first (:borders p))))
                                      (let [above (get-in placed [(dec y) x :borders 2])]
                                        (fn [p]
                                          (= above (first (:borders p))))))
                        match-left (if (= 0 x)
                                     (fn [p]
                                       (is-unique (get (:borders p) 3)))
                                     (let [to-the-left (get-in placed [y (dec x) :borders 1])]
                                       (fn [p]
                                         (= to-the-left (get (:borders p) 3)))))
                        match (fn [p]
                                (and (match-above p) (match-left p)))
                        next-piece (->> jumble
                                        (filter match)
                                        first)]
                    (recur (update placed y (fnil conj []) next-piece)
                           (remove (fn [p] (= (:id p) (:id next-piece))) jumble)
                           (inc n)))))
        find-sea-monsters (fn [image]
                            (->> image
                                 (partition 3 1)
                                 (mapcat (fn [lines]
                                           (->> lines
                                                (map #(partition 20 1 %))
                                                (apply map vector)
                                                (map (fn [block]
                                                       (map (fn [line] (apply str line)) block))))))
                                 (filter (fn [[l1 l2 l3]]
                                           (and (re-matches #"..................#." l1)
                                                (re-matches #"#....##....##....###" l2)
                                                (re-matches #".#..#..#..#..#..#..." l3))))
                                 count))
        all-image-flips (->> [image (flip-image image)]
                             (mapcat (fn [im] (take 4 (iterate rotate-image image)))))
        sea-monsters (reduce max (map find-sea-monsters all-image-flips))]
    (- (->> (apply concat image)
            (filter #{\#})
            count)
       (* sea-monsters 15))))
