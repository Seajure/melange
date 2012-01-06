(ns leiningen.melange
  (:refer-clojure :exclude [test])
  (:require [leiningen.test :as test]
            [leiningen.core :as core]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.java.shell :as sh]))

(defonce project-graph (read-string (slurp (io/resource "project_graph.clj"))))

(defn show [project entry]
  (doseq [dependent (:dependents entry)]
    (println "==" dependent ":" (-> dependent
                                    project-graph
                                    :github-url))))

(defn clone [project entry]
  (.mkdirs (io/file (:root project) "dependents"))
  (doseq [dependent (:dependents entry)
          :let [url (:github-url (project-graph dependent))]
          :when url]
    (println "Cloning" dependent "...")
    (let [retval (sh/sh "git" "clone"
                        (str url ".git")
                        (str "dependents/" (name dependent)))]
      (when (pos? (:exit retval))
        (println "Clone failed:")
        (println (:out retval))
        (println (:err retval))))))

(defn has-tests? [project]
  (and (not (:eval-in-leiningen project))
       (seq (filter #(.endsWith (.getName %) ".clj")
                    (file-seq (io/file (:test-path project)))))))

(defn test-project [project summary dependent]
  (println "Running tests for" dependent)
  (let [project (core/read-project
                 (str (io/file (:root project) "dependents"
                               (name dependent) "project.clj")))]
    (if (has-tests? project)
      (assoc summary dependent (zero? (try (test/test project)
                                           (catch Exception e
                                             (println (.getMessage e))
                                             1))))
      (do (println dependent "appears to have no tests.")
          summary))))

(defn test [project entry]
  (let [dependents (.list (io/file (:root project) "dependents"))
        results (reduce (partial test-project project) {} dependents)]
    (println "Test results:")
    (pprint/pprint results)
    (println (count (filter val results)) "passed out of" (count dependents))))

(defn melange [project command & args]
  (let [name (-> project :name keyword)
        entry (project-graph name)]
    (if-let [command-fn (ns-resolve 'leiningen.melange (symbol command))]
      (apply command-fn project entry args)
      (println "Melange subcommand not found."))))
