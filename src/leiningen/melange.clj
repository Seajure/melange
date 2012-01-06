(ns leiningen.melange
  (:refer-clojure :exclude [test])
  (:require [leiningen.test :as test]
            [leiningen.core :as core]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.java.shell :as sh]))

(defonce project-graph (read-string (slurp (io/resource "project_graph.clj"))))

(defn show [project entry]
  (doseq [dep-name (:dependents entry)
          :let [dep (project-graph dep-name)]]
    (println "==" (name dep-name))
    (doseq [k [:description :homepage :github-url]
            :when (not (string/blank? (dep k)))]
      (println "  " (dep k)))))

(defn clone [project entry]
  (.mkdirs (io/file (:root project) "dependents"))
  (doseq [dependent (:dependents entry)
          :let [url (:github-url (project-graph dependent))]
          :when url]
    (println "Cloning" dependent "...")
    (let [retval (sh/sh "git" "clone" (str url ".git")
                        (str "dependents/" (name dependent)))]
      (when (pos? (:exit retval))
        (println "Clone failed:" (:out retval) (:err retval))))))

(defn has-tests? [project]
  (and (not (:eval-in-leiningen project))
       (seq (filter #(.endsWith (.getName %) ".clj")
                    (file-seq (io/file (:test-path project)))))))

;; TODO: detect other testing frameworks
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

(defn test-print-summary [results total]
  (println "Test results:")
  (doseq [[dependent passed?] results]
    (println " " dependent " - "(if passed? "pass" "FAIL")))
  (println (count (filter val results)) "passed out of" total))

(defn test [project entry]
  (let [dependents (.list (io/file (:root project) "dependents"))
        results (reduce (partial test-project project) {} dependents)]
    (if (seq dependents)
      (test-print-summary results (count dependents))
      (println "No dependent clones present; run \"lein melange clone\"."))))

(defn melange
  "Operate on downstream dependents.

Commands: show, clone, test"
  [project command & args]
  (let [name (-> project :name keyword)
        entry (project-graph name)]
    (if-let [command-fn (ns-resolve 'leiningen.melange (symbol command))]
      (apply command-fn project entry args)
      (println "Melange subcommand not found."))))
