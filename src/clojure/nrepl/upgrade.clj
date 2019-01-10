(ns nrepl.upgrade
  (:require [clojure.java.io :as io]))

;; TODO: dedup with base64 in elisions branch once both are merged 
(defn base64-encode [^java.io.InputStream in]
  (let [table "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
        sb (StringBuilder.)]
    (loop [shift 4 buf 0]
      (let [got (.read in)]
        (if (neg? got)
          (do
            (when-not (= shift 4)
              (let [n (bit-and (bit-shift-right buf 6) 63)]
                (.append sb (.charAt table n))))
            (cond
              (= shift 2) (.append sb "==")
              (= shift 0) (.append sb \=))
            (str sb))
          (let [buf (bit-or buf (bit-shift-left got shift))
                n (bit-and (bit-shift-right buf 6) 63)]
            (.append sb (.charAt table n))
            (let [shift (- shift 2)]
              (if (neg? shift)
                (do
                  (.append sb (.charAt table (bit-and buf 63)))
                  (recur 4 0))
                (recur shift (bit-shift-left buf 6))))))))))

(defn base64-decode [^String s]
  (let [table "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
        in (java.io.StringReader. s)
        bos (java.io.ByteArrayOutputStream.)]
    (loop [bits 0 buf 0]
      (let [got (.read in)]
        (when-not (or (neg? got) (= 61 #_\= got))
          (let [buf (bit-or (.indexOf table got) (bit-shift-left buf 6))
                bits (+ bits 6)]
            (if (<= 8 bits)
              (let [bits (- bits 8)]
                (.write bos (bit-shift-right buf bits))
                (recur bits (bit-and 63 buf)))
              (recur bits buf))))))
    (.toByteArray bos)))

(defn- lexicographically
  [#^"[B" a #^"[B" b]
  (let [alen (alength a)
        blen (alength b)
        len  (min alen blen)]
    (loop [i 0]
      (if (== i len)
        (- alen blen)
        (let [x (- (int (aget a i)) (int (aget b i)))]
          (if (zero? x)
            (recur (inc i))
            x))))))

(defn- ^String as-string [x]
  (str (if (keyword? x) (symbol x) x)))

(defn- print-bencode-utf8
  "bencode over utf8 stream"
  ([x] (print-bencode-utf8 *out* x))
  ([^java.io.Writer out x]
    (cond
      (integer? x) (doto out (.write (int \i)) (.write (str x)) (.write (int \e)))
      (or (string? x) (keyword? x) (symbol? x))
      (let [s (as-string x)
            n (count (.getBytes s "UTF-8"))]
        (doto out
          (.write (str n))
          (.write (int \:))
          (.write s)))
      (map? x)
      (let [a (to-array (for [[k v] x] [(.getBytes (as-string k) "UTF-8") v]))
            a (sort-by first lexicographically a)] ; beware: mutable sort
        (.write out (int \d))
        (doseq [[^bytes k v] a]
          (print-bencode-utf8 out (String. k "UTF-8")) (print-bencode-utf8 out v))
        (.write out (int \e)))
      (or (nil? x) (seq? x) (coll? x))
      (do
        (.write out (int \l))
        (doseq [x x] (print-bencode-utf8 out x))
        (.write out (int \e)))
      :else (throw (ex-info (str "Unexpected value passed to print-bencode-utf8: " x) {:v x})))))

(defn- read-bencode-int-utf8 [^java.io.PushbackReader in]
  (loop [n 0]
    (let [c (.read in)
          d (- c (int \0))]
      (if (<= 0 d 9)
        (recur (+ (* 10 n) d))
        (do
          (.unread in c)
          n)))))

(defn- read-bencode-string-utf8 [^java.io.PushbackReader in]
  (let [n (read-bencode-int-utf8 in)
        sb (StringBuilder. n)]
    (.read in) ; skip \:
    (loop [n n surrogate false]
      (if (zero? n)
        (str sb)
        (let [c (.read in)
              ch (char c)]
          (.append sb ch)
          (cond
            surrogate (recur (- n 4) false)
            (zero? (bit-and -0x80 c)) (recur (- n 1) false)
            (zero? (bit-and -0x800 c)) (recur (- n 2) false)
            (Character/isHighSurrogate ch) (recur n true)
            :else (recur (- n 3) false)))))))

(defn- read-bencode-utf8
  "bencode over utf8 stream"
  ([] (read-bencode-utf8 *in*))
  ([^java.io.PushbackReader in]
    (let [c (.read in)]
      (when-not (neg? c)
        (case (char c)
          (\newline \return) (recur in) ; for terminal experimentation
          \i (let [n (read-bencode-int-utf8 in)] (.read in) n)
          \l (loop [v []]
               (let [c (.read in)]
                 (if (= (int \e) c)
                   v
                   (recur (conj v (read-bencode-utf8 (doto in (.unread c))))))))
          \d (loop [m {}]
               (let [c (.read in)]
                 (if (= (int \e) c)
                   m
                   (recur (assoc m (read-bencode-string-utf8 (doto in (.unread c))) (read-bencode-utf8 in))))))
          (read-bencode-string-utf8 (doto in (.unread c))))))))

(defn classloader
  "Creates a classloader that obey standard delegating policy.
   Takes two arguments: a parent classloader and a function which
   takes a keyword (:resource or :class) and a string (a resource or a class name) and returns an array of bytes
   or nil."
  [parent f]
  (proxy [clojure.lang.DynamicClassLoader] [parent]
    (findResource [name]
      (when-some  [bytes (f :resource name)]
        (let [file (doto (java.io.File/createTempFile "unrepl-sideload-" (str "-" (re-find #"[^/]*$" name)))
                     .deleteOnExit)]
          (io/copy bytes file)
          (-> file .toURI .toURL))))
    (findClass [name]
      (if-some  [bytes (f :class name)]
        (.defineClass ^clojure.lang.DynamicClassLoader this name bytes nil)
        (throw (ClassNotFoundException. name))))))

(defn- boot-nrepl []
  (let [out *out*
        send #(locking out (print-bencode-utf8 out %) (.flush out))
        pending (atom {})
        boot-session "bootloader"
        boot-id (name (gensym "bootloader"))
        cl (.getContextClassLoader (Thread/currentThread))
        cl (classloader cl
             (fn [type name]
               (let [p (promise)
                     type (clojure.core/name type)]
                 (swap! pending assoc [type name] p)
                 (send {"session" boot-session
                        "id" boot-id
                        "status" ["sideloader-lookup"]
                        "type" type
                        "name" name})
                 @p)))]
    (-> #(eval '(do (require 'XXX))) Thread. (doto (.setContextClassLoader cl)) .start)
    (loop []
      (let [{:strs [op id session type name content] :as msg} (read-bencode-utf8)]
        (if (and (= op "sideloader-provide") (= session boot-session))
          (do
            (if-some [p (@pending [type name])]
              (do
                (deliver p (some-> content base64-decode))
                (swap! pending dissoc [type name])
                (send {"id" id "session" session "status" ["done"]}))
              (send {"id" id
                     "session" session
                     "status" ["done" "unexpected-provide"]
                     "type" type
                     "name" name}))
            (recur)))))))
