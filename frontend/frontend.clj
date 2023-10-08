(require '[clojure.java.io :as io])

(def socket (java.net.Socket. "127.0.0.1" 8891))
(def out (.getOutputStream socket))
(def in (.getInputStream socket))
(.write out (byte-array (map #'byte "TEST")))
(.flush out)
(println "Hi")
(let [response (slurp (io/reader in))]
  (println response))