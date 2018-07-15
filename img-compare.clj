(require '[clj-http.client :as client])
(use 'clojure.pprint)

;; ------------------------------------------------- FUNCTIONS ------------------------------------------------- 
;; ------------------------------------------------------------------------------------------------------------- 

(defn write-ds-to-file
  "Saves a Clojure data structure to a file."
  [fname ds]
  (spit fname (with-out-str (pr ds))))

(defn read-ds-from-file
  "Read a Clojure data structure from a file"
  [fname]
  (read-string (slurp fname)))

;; Assuming links.txt contains links to the images we need to compare (they must be stored as a clojure vector).
(def links
  (read-ds-from-file "links.txt"))

(def num-links (count links))

(defn prompt-read
  "Displays a prompt and waits for input."
  [prompt]
  (print (format "%s: " prompt))
  (flush)
  (read-line))

(defn y-or-n?
  "Common lisp's y-or-n-p, but more basic."
  [prompt]
  (= "y"
     (loop []
       (or 
         (re-matches #"[yn]" (.toLowerCase (prompt-read prompt)))
         (recur)))))

(defn try-n-times
  "A simple function that calls another function (f) a number of times (n) in case an exception is thrown."
  [f n]
  (if (zero? n) ;; If (n) reached zero, then the below (f) will be our final attempt (base case).
    (f)
    (try ;; Otherwise call (f) in a try/catch block.
      (f) ;; If there is no exception, then this call to (f) will be the only returned result.
      (catch Throwable e ;; If we caught an exception, then we call try-n-times again with a decreased (n).
        (println (str "Caught exception: " (.getMessage e) " | retrying: " n))
        (try-n-times f (dec n))))))

(defn make-get
  "Makes and HTTP get request and returns a promise for the response.
  Note, that this function will retry the request five times before giving up with an Exception.
  Also there is a timeout of three seconds, after which an exception is thrown."
  [url options]
  (let [prs (promise)]
    (future
      (deliver
        prs (try-n-times #(client/get url (assoc options :socket-timeout 3000 :conn-timeout 3000)) 5))) prs))

(defn get-all-images
  "Pretty much self explanatory. Will return a seq of links and promises for their streams.
  Note, that we will ignore all cookies for each request."
  [img-links]
  (for [link img-links]
    [link (make-get link {:as :stream :client-params {:cookie-policy (constantly nil)}})]))

(defn get-phash
  "Returns the phash of an image."
  [img-stream]
  (.getHash (ImagePHash.) img-stream))

(defn get-similar-images
  "Takes an image link paired with a phash and compares it's similarity against a list of other links and phashes.
  Returns a map of links and hashes with similarity > 90%."
  [hash-pair other-hash-pairs]
  (into {}
        (filter
          #(> (ImagePHash/similarity (second hash-pair) (second %)) 0.90)
          other-hash-pairs)))

(defn make-phash-map
  "Given a list of links, this function returns a map with the links as keys and their phashes as values."
  [img-links]
  (into {} ;; Transform the seq from the next expression into a Clojure map.
        ;; ----------------------------------------------------------------------------------------------------------------------------
        ;; NOTE: At this point in the program we reach a crossroads. We know that get-all-images starts all requests concurrently,
        ;; but we have two ways of processing the requests from there:
        ;; On one hand we could use "map" to go through each request one at a time and do the following:
        ;; 1. Join the request's thread. 2. Compute the requested image's phash.
        ;; This approach requires more time, as it is sequential (it takes ~23 min. to process 1700 imgur pics),
        ;; but it consumes a very reasonable amount of CPU and RAM.
        ;; On the other hand we could use "pmap" which will attempt to do the above two steps concurrently.
        ;; This approach is definitely faster (it takes ~12 min. to process 1700 imgur pics),
        ;; but it has trade-offs when it comes to CPU and RAM usage:
        ;; The problem with "pmap" is that it will attempt to join all threads at once (which will take CPU and RAM).
        ;; It will also attempt to compute the phashes simultaneously (which will take CPU and RAM).
        ;; So basically, you could use "map", which is slower but consumes less resources OR "pmap", which is faster, but more costly.
        ;; ----------------------------------------------------------------------------------------------------------------------------
        (map-indexed ;; This will TRANSFORM the seq of links and img-streams (promises) from get-all-images, INTO a seq of links and phashes.
          (fn [idx pair]
            [(first pair)
             (try
               (println (str "Hashing - " (first pair)))
               (println (str "Progress: " (format "%.2f" (* (/ (double (inc idx)) (double num-links)) 100.0)) "%"))
               (get-phash (:body @(last pair))) ;; If we couldn't hash the img stream, then ImagePHash should throw an exception here.
               (catch Throwable e
                 (println)
                 (println (str "Could not process: " (first pair)))))]) ;; nil will be returned here as the result (no hash).
          (get-all-images img-links))))

(defn get-similarity-map
  "Takes a seq with pairs of image links and phashes and returns a map with all similar links.
  The format is: {link1: [ALL LINKS SIMILAR TO link1], link2: same... }.
  If an image is not similar to any other images, then it's link will contain an empty list, like so: {link1: '()}"
  ([hash-pairs] (get-similarity-map hash-pairs '()))
  ([hash-pairs acc]
   (if (empty? hash-pairs) acc
     (let [head (first hash-pairs)
           tail (into {} (rest hash-pairs))
           similar (keys (get-similar-images head tail))
           rest-without-similar (apply dissoc tail similar)]
       (recur rest-without-similar (conj acc [(first head) similar]))))))

;; ---------------------------------------------------- MAIN ---------------------------------------------------
;; ------------------------------------------------------------------------------------------------------------- 

;; The file hashes.txt will contain the phashes for our images.
(if (= true (y-or-n? "Build new hashes.txt [y/n]?"))
  (write-ds-to-file "hashes.txt" (make-phash-map links)) ;; Fill hashes.txt using make-phash-map.
  (println "Ok, using old hashes.txt file."))

(def hashes
  (read-ds-from-file "hashes.txt"))

(def hashes-clean ;; The make-phash-map function may return nil for some links, if an exception is thrown (best exclude those).
  (filter #(not (nil? (get % 1))) hashes))

(def similarity-map
  (filter ;; Remove images that are not similar to any other image (They should be paired with an empty list).
    #(not (empty? (get % 1))) (get-similarity-map (into {} hashes-clean))))

(pprint similarity-map)
(write-ds-to-file "result.txt" similarity-map)

;; Turn the similarity map into a convenient HTML report.
(def similarity-map-html
  (clojure.string/join
    (letfn [(get-post-url [img-url]
              (clojure.string/replace img-url #"\.jpg" ""))
            (make-link [img-url]
              (str "<a href='" (get-post-url img-url) "'><img src='" img-url "'/></a>"))]
      (for [sim similarity-map]
        (str "<div style='border: double'>"
             (make-link (first sim)) (clojure.string/join " " (map make-link (second sim))) "<br/>"
             "</div>")))))

(spit "report.html" (str "<html><body>" similarity-map-html "</body></html>"))
