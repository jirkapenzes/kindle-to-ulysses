(ns kindle-to-ulysses.core
  (:require [clojure.string :as str]
            [clojure.java.browse :as browse]))

(def group-name "Kindle")
(def clippings-path "/Users/jirkapenzes/dev/projects/kindle-to-ulysses/Clippings.txt")

(defn load-clippings [path] (slurp path))
(defn extract-type [info-line] (get (re-find #"(?:- Your )(Highlight|Bookmark|Note)" info-line) 1))
(defn extract-page [info-line] (get (re-find #"(?:on page )([\d]*)" info-line) 1))
(defn extract-location [info-line] (get (re-find #"(?:Location )([\d]*-[\d]*|[\d]*)" info-line) 1))
(defn extract-date [info-line] (get (re-find #"(?:Added on )([^\n\r]*)" info-line) 1))
(defn split-clippings [clippings] (str/split clippings #"=========="))
(defn not-blank? [str] (not (str/blank? str)))
(defn not-nil? [object] (not (nil? object)))
(defn remove-empty-lines [clip-lines] (filter not-blank? (str/split-lines clip-lines)))

(defn highlight? [clip-lines]
  (if (= (count clip-lines) 3)
    (= (str "Highlight") (extract-type (nth clip-lines 1)))))

(defn parse-clip [clip]
  (let [clip-lines (remove-empty-lines clip)]
    (if (highlight? clip-lines)
      {:book     (nth clip-lines 0)
       :page     (extract-page (nth clip-lines 1))
       :location (extract-location (nth clip-lines 1))
       :date     (extract-date (nth clip-lines 1))
       :clip     (nth clip-lines 2)})))

(defn parse-clippings [clippings]
  (loop [clippings (filter not-nil? (map parse-clip (split-clippings clippings)))
         result (hash-map)]
    (if (empty? clippings)
      result
      (let [clip (first clippings)]
        (recur (rest clippings)
               (update-in result [(:book clip)] conj clip))))))

(defn apply-format-clip [clip]
  (format "\n**Location %s, page %s**\n%s\n----"
          (:location clip) (:page clip) (:clip clip)))

(defn format-book-clippings [book clippings]
  (format "#%s\n%s" book (reduce str (map apply-format-clip clippings))))

(defn format-clippings [clippings]
  (for [book (keys clippings)]
    (let [formatted (format-book-clippings book (get clippings book))]
      (browse/browse-url (str "ulysses://x-callback-url/new-sheet?text=" formatted "&group=" group-name)))))

(defn create-ulysses-group [group-name]
  (browse/browse-url (str "ulysses://x-callback-url/new-group?name=" group-name)))

(defn send-to-ulysses [path]
  (do (create-ulysses-group group-name)
      (-> (load-clippings path)
          (parse-clippings)
          (format-clippings))))

(send-to-ulysses clippings-path)